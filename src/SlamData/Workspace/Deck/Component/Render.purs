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

module SlamData.Workspace.Deck.Component.Render
  ( renderDeck
  , renderError
  ) where

import SlamData.Prelude
import SlamData.Guide as Guide
import Data.Array as A
import Data.List as L
import Halogen as H
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import SlamData.ActionList.Component as ActionList
import SlamData.ActionList.Filter.Component as ActionFilter
import SlamData.Render.CSS as RCSS
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Workspace.Deck.Component.CSS as CSS
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpDialog, cpActionFilter)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query(..))
import SlamData.Workspace.Deck.Component.Query as DCQ
import Utils (endSentence)

renderError ∷ ∀ f a. String → HH.HTML a (f Unit)
renderError err =
  HH.div
    [ HP.classes [ HH.className "sd-workspace-error" ] ]
    [ HH.h1_
        [ HH.text "Couldn't load this SlamData deck." ]
    , HH.p_
        [ HH.text $ endSentence err ]
    ]

renderDeck ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
renderDeck opts deckComponent st =
  HH.div
    (deckClasses st
     ⊕ deckProperties opts
     ⊕ Slider.containerProperties st)
    $ [ HH.div
          [ HP.class_ CSS.deckFrame
          , HE.onMouseDown \ev →
              if st.focused && not (L.null opts.displayCursor)
                then HEH.stopPropagation *> pure (Just (Defocus ev unit))
                else pure Nothing
          ]
          $ frameElements opts st ⊕ [ renderName st.name ]
      , HH.div
          [ HP.class_ CSS.deck
          , HP.key "deck"
          ]
          [ Slider.render opts deckComponent st $ DCS.isFrontSide st.displayMode
            , renderBackside
                $ DCS.isFlipSide st.displayMode
            , renderDialogBackdrop $ DCS.hasDialog st.displayMode
            , renderDialog $ DCS.hasDialog st.displayMode
            ]
      ]
      <> (guard presentFocusDeckHint $> renderFocusDeckHint)
      <> (guard presentFocusDeckFrameHint $> renderFocusDeckFrameHint)

  where
  presentFocusDeckHint ∷ Boolean
  presentFocusDeckHint =
    (opts.accessType ≠ AT.ReadOnly)
      ∧ (not $ L.null opts.displayCursor)
      ∧ (not st.focused)

  presentFocusDeckFrameHint ∷ Boolean
  presentFocusDeckFrameHint =
    (opts.accessType ≠ AT.ReadOnly)
      ∧ (not $ L.null opts.displayCursor)
      ∧ st.focused

  renderFocusDeckHint ∷ DeckHTML
  renderFocusDeckHint =
    Guide.render
      Guide.DownArrow
      (HH.className "sd-focus-deck-hint")
      DCQ.DismissFocusDeckHint
      "This Deck is wrapped in a Dashboard Card and unfocused. To do more with this Deck focus it by clicking or tapping on it."

  renderFocusDeckFrameHint ∷ DeckHTML
  renderFocusDeckFrameHint =
    Guide.render
      Guide.UpArrow
      (HH.className "sd-focus-deck-frame-hint")
      DCQ.DismissFocusDeckFrameHint
      "This Deck is focused. To do more with the Deck containing the Dashboard card this Deck is wrapped in focus it by clicking or tapping on the empty space in this Deck Frame."

  renderDialogBackdrop visible =
    HH.div
      [ HP.classes $ [CSS.dialogBackdrop] ⊕ (guard (not visible) $> CSS.invisible)
      , HE.onClick $ HE.input_ DismissDialog
      , ARIA.hidden $ show $ not visible
      ]
      [ ]

  renderDialog visible =
    HH.div
      [ HP.classes $ [] ⊕ (guard (not visible) $> CSS.invisible)
      , ARIA.hidden $ show $ not visible
      ]
      [ dialogSlot ]

  renderBackside visible =
    HH.div
      [ HP.classes $ [CSS.cardSlider] ⊕ (guard (not visible) $> CSS.invisible)
      , ARIA.hidden $ show $ not visible
      ]
      [ backside ]

deckClasses ∷ ∀ r. DCS.State → Array (HP.IProp (HP.InteractiveEvents (HP.GlobalProperties r)) (Query Unit))
deckClasses st =
  pure $ HP.classes $
    [ CSS.deckContainer
    , HH.className (responsiveSize st.responsiveSize)
    , if st.focused then CSS.focused else CSS.unfocused
    ]

deckProperties ∷ ∀ r. DeckOptions → Array (HP.IProp (HP.InteractiveEvents (HP.GlobalProperties r)) (Query Unit))
deckProperties opts =
  [ HP.key "deck-container"
  , HP.ref (H.action ∘ SetCardElement)
  ] ⊕ (guard (L.length opts.displayCursor <= 1) $>
        HE.onMouseDown \_ → HEH.stopPropagation $> Just (H.action Focus))

renderName ∷ String → DeckHTML
renderName name =
  HH.div
    [ HP.class_ CSS.deckName ]
    [ HH.text name ]

frameElements ∷ DeckOptions → DCS.State → Array DeckHTML
frameElements { accessType, displayCursor } st
  | accessType ≡ AT.ReadOnly = mempty
  | L.null displayCursor = rootFrameElements st
  | otherwise = childFrameElements st

rootFrameElements ∷ DCS.State → Array DeckHTML
rootFrameElements st =
  [ zoomOutButton
  , flipButton
  , deckIndicator st
  ]

childFrameElements ∷ DCS.State → Array DeckHTML
childFrameElements st =
  [ zoomInButton
  , flipButton
  , moveGripper
  , deckIndicator st
  ]

dialogSlot ∷ DeckHTML
dialogSlot =
  HH.slot' cpDialog unit \_ →
    { component: Dialog.comp
    , initialState: H.parentState Dialog.initialState
    }

backside ∷ DeckHTML
backside =
  HH.div
    [ HP.classes [ CSS.card ] ]
    [ HH.div_
        [ HH.div
            [ HP.class_ CCSS.deckCard ]
            [ HH.div
                [ HP.class_ RCSS.deckBackSide ]
                [ HH.slot' cpActionFilter unit \_ →
                    { component: ActionFilter.comp "Filter deck and card actions"
                    , initialState: ActionFilter.initialState
                    }
                , HH.slot' cpBackSide unit \_ →
                    { component: ActionList.comp
                    , initialState: ActionList.initialState []
                    }
                ]
            ]
        ]
    ]

deckIndicator ∷ DCS.State → DeckHTML
deckIndicator st =
  HH.div [ HP.classes [ HH.className "indicator" ] ] $
    A.mapWithIndex renderCircle st.displayCards

  where
  activeCard =
    DCS.activeCardIndex st

  renderCircle ix card =
    HH.i
      [ HP.classes $
          pure case card of
            Right _ → HH.className "available"
            Left DCS.PendingCard → HH.className "pending"
            Left (DCS.ErrorCard _) → HH.className "errored"
            Left (DCS.NextActionCard _) → HH.className "placeholder"
        ⊕ (guard (activeCard ≡ ix) $> HH.className "focused")
      ]
      [ HH.text "" ]

flipButton ∷ DeckHTML
flipButton =
  HH.button
    [ HP.classes [ CSS.flipDeck ]
    , HE.onClick (HE.input_ FlipDeck)
    , ARIA.label "Flip deck"
    , HP.title "Flip deck"
    ]
    []

moveGripper ∷ DeckHTML
moveGripper =
  HH.button
    [ HP.classes [ CSS.grabDeck ]
    , HE.onMouseDown (HE.input GrabDeck)
    , ARIA.label "Grab deck"
    , HP.title "Grab deck"
    ]
    []

zoomInButton ∷ DeckHTML
zoomInButton =
  HH.button
     [ HP.classes [ CSS.zoomDeck ]
     , ARIA.label "Zoom in"
     , HP.title "Zoom in"
     , HE.onClick (HE.input_ ZoomIn)
     ]
     [ glyph B.glyphiconZoomIn ]

zoomOutButton ∷ DeckHTML
zoomOutButton =
  HH.button
   [ HP.classes [ CSS.zoomDeck ]
   , ARIA.label "Zoom out"
   , HP.title "Zoom out"
   , HE.onClick (HE.input_ ZoomOut)
   ]
   [ glyph B.glyphiconZoomOut ]

responsiveSize ∷ DCS.ResponsiveSize → String
responsiveSize = case _ of
  DCS.XSmall  → "sd-size-xsmall"
  DCS.Small   → "sd-size-small"
  DCS.Medium  → "sd-size-medium"
  DCS.Large   → "sd-size-large"
  DCS.XLarge  → "sd-size-xlarge"
  DCS.XXLarge → "sd-size-xxlarge"
