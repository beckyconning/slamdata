{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Draftboard.Component where

import SlamData.Prelude

import Data.Foldable (and, all, find)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Ord (abs)
import Data.Rational (Rational, (%))

import Halogen as H
import Halogen.Component.Utils.Drag as Drag

import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Draftboard.Component.Common (DraftboardDSL, rootRef)
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..))
import SlamData.Workspace.Card.Draftboard.Component.Render (render)
import SlamData.Workspace.Card.Draftboard.Component.State (MoveLocation(..), initialState, modelFromState, updateRect, updateLayout)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (emptyDeck)
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.DOM as DOM

draftboardComponent ∷ CardOptions → CC.CardComponent
draftboardComponent options =
  CC.makeCardComponent CT.Draftboard component options
  where
  component = H.parentComponent
    { render: render options
    , eval: coproduct (evalCard options) (evalBoard options)
    , initialState: const initialState
    , receiver: const Nothing
    }

evalCard ∷ CardOptions → CC.CardEvalQuery ~> DraftboardDSL
evalCard opts = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    map (k ∘ Card.Draftboard ∘ modelFromState) H.get
  CC.Load card next → do
    case card of
      Card.Draftboard model → do
        H.modify (updateLayout model.layout)
        void $ H.queryAll $ H.action DCQ.UpdateCardSize
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    st ← H.get
    recalcRect
    void $ H.queryAll $ H.action DCQ.UpdateCardSize
    pure $ reply High

evalBoard ∷ CardOptions → Query ~> DraftboardDSL
evalBoard opts = case _ of
  SplitStart orientation bias root ev next → do
    H.liftEff $ DOM.stopPropagation (DOM.toEvent ev)
    st ← H.get
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (Splitting drag H.Listening))
    H.modify _ { splitOpts = Just { orientation, bias, root } }
    pure next
  Splitting ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          x = d.x - st.rootRect.left
          y = d.y - st.rootRect.top
          result = do
            { orientation, bias, root } ← st.splitOpts
            { cursor } ← overlapping x y st.cellLayout
            let
              rectR = st.rootRect
              cursor' =
                if not root
                  then fromMaybe Nil (Layout.findSplit orientation cursor st.layout)
                  else Nil
            rs ←
              if List.null cursor'
                then pure Nil
                else getRatios =<< Pane.getAt cursor' st.layout
            rectT ← Layout.absoluteRect rectR <$> Layout.rectAt cursor' st.layout
            let
              relX = d.x - (rectR.left + rectT.left)
              relY = d.y - (rectR.top + rectT.top)
            pure case orientation of
              Orn.Horizontal →
                let
                  ratio = Layout.closestSnapRatio (relX / rectT.width)
                  valid = isSensibleSplit ratio rs && not (isEdge ratio)
                in
                  { orientation
                  , bias
                  , cursor: cursor'
                  , ratio
                  , valid
                  , x: d.x - rectR.left
                  , y: rectT.top
                  , z: rectT.height
                  }
              Orn.Vertical →
                let
                  ratio = Layout.closestSnapRatio (relY / rectT.height)
                  valid = isSensibleSplit ratio rs && not (isEdge ratio)
                in
                  { orientation
                  , bias
                  , cursor: cursor'
                  , ratio
                  , valid
                  , x: rectT.left
                  , y: d.y - rectR.top
                  , z: rectT.width
                  }
        H.modify _ { splitLocation = result }
      Drag.Done _ → do
        let
          result = do
            splitOpts ← st.splitOpts
            loc ← st.splitLocation
            guard loc.valid
            if splitOpts.root
              then pure (Layout.insertRootSplit (Pane.Cell Nothing) loc.orientation loc.ratio loc.bias st.layout)
              else Layout.insertSplit (Pane.Cell Nothing) loc.orientation loc.ratio loc.bias loc.cursor st.layout
        H.modify
          $ updateLayout (fromMaybe st.layout result)
          ∘ _ { splitOpts = Nothing, splitLocation = Nothing }
        H.queryAll $ H.action DCQ.UpdateCardSize
        H.raise CC.modelUpdate
    pure next
  ResizeStart edge ev next → do
    let
      ev' = Drag.mouseEventToPageCoord ev
      loc =
        { edge
        , ratio: edge.ratio
        , valid: true
        , collapse: Nothing
        , offset: 0.0
        , initial: case edge.orientation of
            Orn.Horizontal → ev'.pageX
            Orn.Vertical   → ev'.pageY
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (Resizing drag H.Listening))
    H.modify _ { resizeLocation = Just loc }
    pure next
  Resizing ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          rectR = st.rootRect
          result = do
            loc   ← st.resizeLocation
            rectT ← Layout.absoluteRect rectR <$> Layout.rectAt loc.edge.parent st.layout
            sibs  ← Pane.children <$> Pane.getAt loc.edge.parent st.layout
            let
              ix   = loc.edge.index + 1
              relX = d.x - (rectR.left + rectT.left)
              relY = d.y - (rectR.top + rectT.top)
              pre  = List.take ix sibs
              post = List.drop ix sibs
            pure case loc.edge.orientation of
              Orn.Horizontal →
                let
                  ratio = Layout.closestSnapRatio (relX / rectT.width)
                  collapse = isCollapsing ratio pre post
                  valid = not (isEdge ratio) || isJust collapse
                in loc
                  { offset = d.x - loc.initial
                  , ratio = ratio
                  , valid = valid
                  , collapse = collapse
                  }
              Orn.Vertical →
                let
                  ratio = Layout.closestSnapRatio (relY / rectT.height)
                  collapse = isCollapsing ratio pre post
                  valid = not (isEdge ratio) || isJust collapse
                in loc
                  { offset = d.y - loc.initial
                  , ratio = ratio
                  , valid = valid
                  , collapse = collapse
                  }
        H.modify _ { resizeLocation = result }
      Drag.Done _ → do
        let
          result = do
            loc ← st.resizeLocation
            guard loc.valid
            Layout.resizeEdge' loc.edge.index loc.ratio loc.edge.parent st.layout
        H.modify
          $ updateLayout (fromMaybe st.layout result)
          ∘ _ { resizeLocation = Nothing }
        H.queryAll $ H.action DCQ.UpdateCardSize
        H.raise CC.modelUpdate
    pure next
  DeleteCell cursor next → do
    st ← H.get
    let
      result = Layout.deleteCell cursor st.layout
    H.modify (updateLayout (fromMaybe st.layout result))
    H.queryAll $ H.action DCQ.UpdateCardSize
    H.raise CC.modelUpdate
    pure next
  GrabStart deckId ev next → do
    st ← H.get
    for_ (Map.lookup deckId st.cursors) \cursor →
      H.subscribe $ Drag.dragEventSource ev \drag →
        Just (right (Grabbing (deckId × cursor) drag H.Listening))
    pure next
  Grabbing (deckId × cursor) ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          x = d.x - st.rootRect.left
          y = d.y - st.rootRect.top
        case overlapping x y st.cellLayout of
          Just cell@{ value: Nothing } →
            H.modify _ { moveLocation = Just (Move cell) }
          Just cell@{ value: (Just deckId'), rect } | deckId /= deckId' → do
            let
              relX = Int.floor (x - rect.left) % Int.floor rect.width
              relY = Int.floor (y - rect.top) % Int.floor rect.height
              move = case relX < 1%2, relY < 1%2 of
                xa, ya | xa && ya && relY < relX   → Group cell Orn.Vertical Layout.SideA
                xa, ya | xa && ya                  → Group cell Orn.Horizontal Layout.SideA
                xa, ya | xa && one - relY < relX   → Group cell Orn.Vertical Layout.SideB
                xa, ya | xa                        → Group cell Orn.Horizontal Layout.SideA
                xa, ya | ya && relY < one - relX   → Group cell Orn.Vertical Layout.SideA
                xa, ya | ya                        → Group cell Orn.Horizontal Layout.SideB
                xa, ya | one - relY < one - relX   → Group cell Orn.Vertical Layout.SideB
                _ , _                              → Group cell Orn.Horizontal Layout.SideB
            H.modify _ { moveLocation = Just move }
          _ →
            H.modify _ { moveLocation = Just (Floating x y) }
      Drag.Done _ → do
        for_ st.moveLocation case _ of
          Floating _ _ →
            H.modify _ { moveLocation = Nothing }
          Group cell orn bias →
            for_ cell.value \deckId' → do
              H.modify _ { moveLocation = Nothing }
              groupDeck orn bias deckId deckId'
          Move cell → do
            let
              result =
                pure st.layout
                  >>= Pane.modifyAt (const (Pane.Cell Nothing)) cursor
                  >>= Pane.modifyAt (const (Pane.Cell (Just deckId))) cell.cursor
            H.modify
              $ updateLayout (fromMaybe st.layout result)
              ∘ _ { moveLocation = Nothing }
            void $ H.query deckId $ H.action DCQ.UpdateCardSize
        H.raise CC.modelUpdate
    pure next
  AddDeck cursor next → do
    addDeck opts cursor
    H.raise CC.modelUpdate
    pure next
  PreventDefault ev next →
    H.liftEff (DOM.preventDefault ev) $> next

isCollapsing
  ∷ ∀ a
  . Rational
  → List (Pane.Pane (Maybe a))
  → List (Pane.Pane (Maybe a))
  → Maybe Layout.SplitBias
isCollapsing ratio pre post
  | ratio == zero && and (map (all isNothing) pre) = Just Layout.SideA
  | ratio == one && and (map (all isNothing) post) = Just Layout.SideB
  | otherwise = Nothing

isSensibleSplit ∷ Rational → List (Rational) → Boolean
isSensibleSplit ratio = go zero
  where
  go off Nil = true
  go off (r : xs) =
    if abs (ratio - off) < 1%16
      then false
      else go (off + r) xs

getRatios ∷ ∀ a . Pane.Pane a → Maybe (List Rational)
getRatios (Pane.Split _ ps) = Just (map fst ps)
getRatios _ = Nothing

isEdge ∷ Rational → Boolean
isEdge r = r == zero || r == one

overlapping
  ∷ ∀ a
  . Number
  → Number
  → List (Layout.Cell a Number)
  → Maybe (Layout.Cell a Number)
overlapping x y = find go
  where
  go { rect: { top, left, width, height } } =
    y >= top && x >= left && y < top + height && x < left + width

recalcRect ∷ DraftboardDSL Unit
recalcRect = do
  H.getHTMLElementRef rootRef >>= traverse_ \root → do
    rect ← H.liftEff (DOM.getOffsetClientRect root)
    H.modify (updateRect rect)

addDeck ∷ CardOptions → Pane.Cursor → DraftboardDSL Unit
addDeck opts cursor = do
  deckId × _ ← H.lift $ P.freshDeck emptyDeck (ED.Completed Port.emptyOut)
  H.lift $ P.linkToParent opts.cardId deckId
  st ← H.get
  let
    layout = Pane.modifyAt (const (Pane.Cell (Just deckId))) cursor st.layout
  H.modify
    $ updateLayout (fromMaybe st.layout layout)
    ∘ _ { inserting = false }
  H.lift $ Wiring.focusDeck deckId

groupDeck
  ∷ Orn.Orientation
  → Layout.SplitBias
  → DeckId
  → DeckId
  → DraftboardDSL Unit
groupDeck orn bias deckFrom deckTo = do
  mbActive ← H.query deckTo (H.request DCQ.GetActiveCard)
  for_ mbActive case _ of
    Just coord → H.lift $ P.groupDeck orn bias deckFrom deckTo coord
    Nothing → H.lift $ P.wrapAndGroupDeck orn bias deckFrom deckTo
