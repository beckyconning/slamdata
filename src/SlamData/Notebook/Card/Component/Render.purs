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

module SlamData.Notebook.Card.Component.Render
  ( CardHTML
  , header
  , statusBar
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Int (fromNumber)
import Data.Lens (preview)
import Data.Time (Seconds(..), Milliseconds(..), toSeconds)
import Data.Visibility (Visibility(..))

import Halogen (ParentHTML)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Notebook.Card.Component.Query (CardQuery(..), InnerCardQuery)
import SlamData.Notebook.Card.Component.State
  (CardState, AnyCardState)
import SlamData.Notebook.Card.RunState (RunState(..), isRunning)
import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS
import SlamData.Notebook.Card.Port (_Blocked)
import SlamData.Notebook.Card.CardType (CardType, cardName, cardGlyph, controllable)

type CardHTML = ParentHTML AnyCardState CardQuery InnerCardQuery Slam Unit

header
  ∷ CardType
  → CardState
  → Array CardHTML
header cty cs =
  guard (controllable cty) $>
    H.div
      [ P.classes [CSS.cardHeader]
      , ARIA.label $ (cardName cty) ⊕ " card"
      ]
      [ H.div
          [ P.class_ CSS.cardIcon ]
          [ cardGlyph cty ]
      , H.div
          [ P.class_ CSS.cardName ]
          [ H.text $ cardName cty ]
      , controls cs
      ]

cardBlocked ∷ CardState → Boolean
cardBlocked cs =
  isJust $ cs.input >>= preview _Blocked


controls ∷ CardState → CardHTML
controls cs =
  H.div
    [ P.classes [B.pullRight, CSS.cardControls] ]
    $ (guard (not $ cardBlocked cs))
       ≫ [ H.button
           [ P.title cardOptionsLabel
           , ARIA.label cardOptionsLabel
           , E.onClick (E.input_ ToggleCollapsed)
           ]
           [ glyph if cs.isCollapsed
                     then B.glyphiconEyeOpen
                     else B.glyphiconEyeClose
           ]
         , H.button
           [ P.title "Delete card"
           , ARIA.label "Delete card"
           , E.onClick (E.input_ TrashCard)
           ]
           [ glyph B.glyphiconTrash ]
         ]
    ⊕ [ glyph B.glyphiconChevronLeft ]

  where
  cardOptionsLabel ∷ String
  cardOptionsLabel =
    if cs.isCollapsed
      then "Show card options"
      else "Hide card options"


statusBar ∷ Boolean → CardState → CardHTML
statusBar hasResults cs =
  H.div
    [ P.classes [CSS.cardEvalLine] ]
    $ [ H.button
          [ P.classes [B.btn, B.btnPrimary, button.className]
          , E.onClick (E.input_ $ if isRunning cs.runState
                                  then StopCard
                                  else RunCard)
          , P.title button.label
          , P.disabled $ cardBlocked cs
          , ARIA.label button.label
          ]
          [ glyph button.glyph ]
      , H.div
          [ P.class_ CSS.statusText ]
          [ H.text $ if cardBlocked cs
                       then ""
                       else runStatusMessage cs.runState ]
      , H.div
          [ P.classes [B.pullRight, CSS.cardControls] ]
          $ A.catMaybes
              [ toggleMessageButton cs
              , if hasResults then Just linkButton else Nothing
              , Just $ glyph B.glyphiconChevronLeft
              ]
      ]
    ⊕ statusMessages cs
  where

  button =
    if isRunning cs.runState
    then { className: CSS.stopButton, glyph: B.glyphiconStop, label: "Stop" }
    else { className: CSS.playButton, glyph: B.glyphiconRefresh, label: "Refresh" }

runStatusMessage ∷ RunState → String
runStatusMessage RunInitial = ""
runStatusMessage (RunElapsed t) =
  "Running for " ⊕ printSeconds t ⊕ "..."
runStatusMessage (RunFinished t) =
  "Finished: took " ⊕ printMilliseconds t ⊕ "."

printSeconds ∷ Milliseconds → String
printSeconds t = case toSeconds t of
  Seconds s → maybe "0" show (fromNumber (Math.floor s)) ⊕ "s"

printMilliseconds ∷ Milliseconds → String
printMilliseconds (Milliseconds ms) =
  maybe "0" show (fromNumber (Math.floor ms)) ⊕ "ms"

refreshButton ∷ CardHTML
refreshButton =
  H.button
    [ P.title "Refresh card content"
    , ARIA.label "Refresh card content"
    , P.classes [CSS.refreshButton]
    , E.onClick (E.input_ RefreshCard)
    ]
    [ glyph B.glyphiconRefresh ]

toggleMessageButton ∷ CardState → Maybe CardHTML
toggleMessageButton { messages, messageVisibility } =
  if A.null messages
  then Nothing
  else Just $
    H.button
      [ P.title label
      , ARIA.label label
      , E.onClick (E.input_ ToggleMessages)
      ]
      [ glyph if isCollapsed then B.glyphiconEyeOpen else B.glyphiconEyeClose ]
  where
  label = if isCollapsed then "Show messages" else "Hide messages"
  isCollapsed = messageVisibility ≡ Invisible

linkButton ∷ CardHTML
linkButton =
  H.button
    [ P.title "Embed card output"
    , ARIA.label "Embed card output"
    , E.onClick (E.input_ ShareCard)
    ]
    [ H.span [ P.class_ CSS.shareButton ] [] ]

statusMessages ∷ CardState → Array (CardHTML)
statusMessages cs@{ messages, messageVisibility }
  | cardBlocked cs =
      [ H.div
        [ P.classes [ CSS.cardBlockedMessage ] ]
        [ H.div_ [ H.text "There are errors in parent cards" ] ]]
  | A.null messages = []
  | otherwise =
      [ H.div
          [ P.classes classes ]
           $ [ H.div_ failureMessage ]
           ⊕ if isCollapsed
               then []
               else map (either message message) messages'
      ]
  where
  isCollapsed = messageVisibility ≡ Invisible
  errorMessages = let es = A.filter isLeft messages in es
  hasErrors = not (A.null errorMessages)
  messages' = if hasErrors then errorMessages else messages
  classes =
    let
      cls = if hasErrors
              then CSS.cardFailures
              else CSS.cardMessages
    in
      if isCollapsed then [cls, CSS.collapsed] else [cls]
  failureMessage =
    if hasErrors
      then
        let
          numErrors = A.length errorMessages
          s = if numErrors ≡ 1 then "" else "s"
        in
          [H.text $ show numErrors ⊕ " error" ⊕ s ⊕ " during evaluation. "]
    else []

message ∷ String → CardHTML
message = H.pre_ ∘ pure ∘ H.text
