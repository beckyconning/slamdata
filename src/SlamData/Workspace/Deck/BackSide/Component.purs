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

module SlamData.Workspace.Deck.BackSide.Component where

import SlamData.Prelude

import Data.Array as A
import Data.List (List(..), (:))
import Data.Foldable as F

import Halogen as H
import Halogen.Component.ChildPath (cpI)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Component.Utils as HU

import SlamData.ActionList.Component as ActionList
import SlamData.Monad (Slam)
import SlamData.Quasar as Api
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Deck.Component.State (CardDef)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Eval.Persistence as P

data Query a
  = DoAction BackAction a
  | UpdateCard (Maybe CardDef) (Array CardDef) a

data BackAction
  = Trash
  | Rename
  | Embed
  | Publish
  | DeleteDeck
  | Mirror
  | Wrap
  | Unwrap
  | Share
  | Unshare

type BackSideOptions =
  { deckId ∷ DeckId
  , displayCursor ∷ List DeckId
  }

type State = Unit

type StateP =
  H.ParentState
    State
    (ActionList.State BackAction)
    Query
    (ActionList.Query BackAction)
    Slam
    Unit

type HTML =
  H.ParentHTML
    (ActionList.State BackAction)
    Query
    (ActionList.Query BackAction)
    Slam
    Unit

type DSL =
  H.ParentDSL
    State
    (ActionList.State BackAction)
    Query
    (ActionList.Query BackAction)
    Slam
    Unit

type QueryP = Query ⨁ H.ChildF Unit (ActionList.Query BackAction)

derive instance eqBackAction ∷ Eq BackAction

allBackActions ∷ Boolean → Array BackAction
allBackActions isAdvanced =
  [ Trash
  , Rename
  ]
  ⊕ (if isAdvanced
      then [ Share
           , Unshare
           ]
      else [ ])
  ⊕ [ Embed
    , Publish
    , DeleteDeck
    , Mirror
    , Wrap
    , Unwrap
    ]

initialState ∷ State
initialState = unit

labelAction ∷ BackAction → ActionList.ActionName
labelAction =
  ActionList.ActionName
    ∘ case _ of
        Trash → "Delete card"
        Rename → "Rename deck"
        Share → "Share deck"
        Embed → "Embed deck"
        Publish → "Publish deck"
        DeleteDeck → "Delete deck"
        Mirror → "Mirror"
        Wrap → "Wrap"
        Unwrap → "Collapse"
        Unshare → "Unshare deck"

actionDescripton ∷ BackAction → ActionList.ActionDescription
actionDescripton =
  ActionList.ActionDescription
    ∘ case _ of
        Trash → "Delete card"
        Rename → "Rename deck"
        Share → "Share deck"
        Embed → "Embed deck"
        Publish → "Publish deck"
        DeleteDeck → "Delete deck"
        Mirror → "Mirror"
        Wrap → "Wrap"
        Unwrap → "Collapse"
        Unshare → "Unshare deck"

actionHighlighted ∷ Boolean → Maybe CardDef → Array CardDef → BackAction → ActionList.ActionHighlighted
actionHighlighted unwrappable activeCard cardDefs a =
  ActionList.ActionHighlighted
    $ case activeCard, a of
      Nothing, Trash → false
      _, Unwrap → unwrappable
      _, Mirror | F.elem CT.Draftboard (_.cardType <$> cardDefs) → false
      _, _ → true

actionIcon ∷ BackAction → ActionList.ActionIconSrc
actionIcon =
  ActionList.ActionIconSrc
    ∘ case _ of
        Trash → "img/cardAndDeckActions/deleteCard.svg"
        Rename → "img/cardAndDeckActions/renameDeck.svg"
        Share → "img/cardAndDeckActions/shareDeck.svg"
        Unshare → "img/cardAndDeckActions/unshareDeck.svg"
        Embed → "img/cardAndDeckActions/embedDeck.svg"
        Publish → "img/cardAndDeckActions/publishDeck.svg"
        Mirror → "img/cardAndDeckActions/mirrorDeck.svg"
        Wrap → "img/cardAndDeckActions/wrapDeck.svg"
        Unwrap → "img/cardAndDeckActions/unwrapDeck.svg"
        DeleteDeck → "img/cardAndDeckActions/deleteDeck.svg"

toActionListAction ∷ Boolean → Maybe CardDef → Array CardDef → BackAction → ActionList.Action BackAction
toActionListAction unwrappable activeCard cardDefs backAction =
   ActionList.Do
     (labelAction backAction)
     (actionIcon backAction)
     (actionDescripton backAction)
     (actionHighlighted unwrappable activeCard cardDefs backAction)
     backAction

comp ∷ BackSideOptions → H.Component StateP QueryP Slam
comp opts =
  H.parentComponent
    { render: const render
    , eval: eval opts
    , peek: Just (peek ∘ H.runChildF)
    }

render ∷ HTML
render =
  -- Extra div for consistent targetting with next action card styles
  HH.div_
    [ HH.div
        [ HP.class_ CCSS.deckCard ]
        [ HH.div
            [ HP.class_ CSS.deckBackSide ]
            [ HH.slot' cpI unit \_ →
                { component: ActionList.comp
                , initialState: ActionList.initialState []
                }
            ]
        ]
    ]

eval ∷ BackSideOptions → Query ~> DSL
eval opts = case _ of
  DoAction _ next →
    pure next
  UpdateCard card defs next → do
    uw ← fromMaybe false <$> traverse (calculateUnwrappable opts) card
    isAdvanced ← isRight <$> Api.retrieveAuthProviders
    void
      $ H.query' cpI unit
      $ H.action
      $ ActionList.UpdateActions
      $ toActionListAction uw card defs
      <$> allBackActions isAdvanced
    pure next

calculateUnwrappable ∷ BackSideOptions → CardDef → DSL Boolean
calculateUnwrappable { displayCursor, deckId } { cardId } = do
  deck ← map _.model <$> (H.liftH $ H.liftH (P.getDeck deckId))
  card ← map _.model <$> (H.liftH $ H.liftH (P.getCard cardId))
  let
    cardLen = A.length ∘ _.cards <$> deck
    deckIds = CM.childDeckIds <$> card
  pure case displayCursor, card, cardLen, deckIds of
    Nil    , Just (CM.Draftboard _), Just 1, Just (_ : Nil) → true
    _ : Nil, Just (CM.Draftboard _), Just 1, _ → true
    _ , _, _, _ → false

peek ∷ ∀ a. ActionList.Query BackAction a → DSL Unit
peek = case _ of
  ActionList.Selected action _ →
    case action of
      ActionList.DoInternal _ _ _ _ backAction →
        HU.raise' $ H.action $ DoAction backAction
      _ →
        pure unit
  _ → pure unit
