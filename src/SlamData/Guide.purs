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
module SlamData.Guide where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

data Arrow = RightArrow | DownArrow

arrowClassName ∷ Arrow → HH.ClassName
arrowClassName =
  case _ of
    RightArrow -> HH.className "sd-guide-right-arrow"
    DownArrow -> HH.className "sd-guide-down-arrow"

render ∷ ∀ f a. Arrow → HH.ClassName → (Unit → f Unit) → String → HH.HTML a (f Unit)
render arrow className dismissQuery text =
  HH.div
    [ HP.classes [ HH.className "sd-guide", className ] ]
    [ HH.div
        [ HP.classes
            [ HH.className "sd-notification"
            , arrowClassName arrow
            ]
        ]
        [ HH.div
            [ HP.class_ $ HH.className "sd-notification-text" ]
            [ HH.text text ]
        , HH.div
            [ HP.class_ $ HH.className "sd-notification-buttons" ]
            [ HH.button
                [ HP.classes [ HH.className "sd-notification-dismiss" ]
                , HE.onClick (HE.input_ dismissQuery)
                , ARIA.label "Dismiss"
                ]
                [ HH.text "×" ]
            ]
        ]
    ]

renderStepByStepWithTupleArray
  ∷ ∀ f a
  . { next ∷ Unit → f Unit, dismiss ∷ Unit → f Unit }
  → Maybe Int
  → Array (Tuple String String)
  → Array (HH.HTML a (f Unit))
renderStepByStepWithTupleArray queries stepIndex steps =
  maybe [] pure do
    index ← stepIndex
    step ← Array.index steps index
    pure
      $ renderStepByStep
          queries
          (fst step)
          (snd step)
          (index == Array.length steps - 1)

renderStepByStep
  ∷ ∀ f a
  . { next ∷ Unit → f Unit, dismiss ∷ Unit → f Unit }
  → String
  → String
  → Boolean
  → HH.HTML a (f Unit)
renderStepByStep queries imageUri text last =
  HH.div
    [ HE.onClick \_ → EH.stopPropagation $> Just (H.action $ queries.dismiss)
    , HP.class_ $ HH.className "sd-step-by-step-guide-backdrop"
    ]
    [ HH.div
      [ HP.class_ $ HH.className "sd-step-by-step-guide"
      , HE.onClick (\_ → EH.stopPropagation $> Nothing)
      ]
      ([ HH.img [ HP.src imageUri ] , HH.p_ [ HH.text text ] ] <> pure buttons)
    ]
      where
      buttons =
        HH.div_
          $ if last
            then
              [ HH.button
                  [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.dismiss) ]
                  [ HH.text "Get started!" ]
              ]
            else
              [ HH.button
                 [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.next) ]
                 [ HH.text "Next" ]
              , HH.button
                 [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.dismiss) ]
                  [ HH.text "Skip" ]
              ]

preloadStepByStepWithTupleArray ∷ ∀ a f. Array (Tuple String String) → HH.HTML a f
preloadStepByStepWithTupleArray steps =
  HH.div
    [ ARIA.hidden "true" ]
    ((\url → HH.img [ HP.src url ]) <<< fst <$> steps)

cardGuideSteps ∷ Array (Tuple String String)
cardGuideSteps =
  [ Tuple
      "img/cardGuide/1.gif"
      "Welcome to SlamData! When using SlamData we think about analytics in terms of cards."
  , Tuple
      "img/cardGuide/2.gif"
      "Each card performs a function like showing a pie chart or opening a data set."
  , Tuple
      "img/cardGuide/3.gif"
      "Each card passes an output to the card after it, for example passing a data set to a chart."
  , Tuple
      "img/cardGuide/4.gif"
      "This is done by stacking cards ontop of each other to build decks. Decks represent analytic workflows."
  , Tuple
      "img/cardGuide/5.gif"
      "Check out the deck backside, slide the card grippers and visit the next action card to see more of what's possible with SlamData."
  ]

flipGuideSteps ∷ Array (Tuple String String)
flipGuideSteps =
  [ Tuple
      "img/flipGuide/1.gif"
      "Welcome to the flip side of your deck. The flip side lists actions for the deck and current card."
  , Tuple
      "img/flipGuide/2.gif"
      "Delete Card deletes the current card and any cards inserted after it. Error cards and next action cards aren't inserted so are not deleted."
  , Tuple
      "img/flipGuide/3.gif"
      "Wrap Deck places the deck on a draftboard card in a new deck."
  , Tuple
      "img/flipGuide/4.gif"
      "Draftboard cards allow muliple decks to be laid out on one card. They are perfect for creating dashboards."
  , Tuple
      "img/flipGuide/5.gif"
      "Decks can be removed using Delete Deck."
  , Tuple
      "img/flipGuide/6.gif"
      "Mirror Deck creates a live copy of the cards in the deck and places them in a new deck on the current draftboard card."
  , Tuple
      "img/flipGuide/7.gif"
      "Any changes made to the original or mirrored cards are reflected in each other except for deletion."
  , Tuple
      "img/flipGuide/8.gif"
      "Adding cards to either deck will not affect the other. Mirroring is great for adding form controls to your dashboard."
  , Tuple
      "img/flipGuide/9.gif"
      "Sharing is easy and secure with SlamData Advanced."
  , Tuple
      "img/flipGuide/10.gif"
      "Share deck allows decks to be shared for viewing or editing with people and groups. It also allows for the creation of access tokens."
  , Tuple
      "img/flipGuide/11.gif"
      "Access can be managed or revoked using Unshare Deck"
  , Tuple
      "img/flipGuide/12.gif"
      "With both SlamData and SlamData Advanced decks can be embedded on web and intranet sites using Embed Deck."
  , Tuple
      "img/flipGuide/13.gif"
      "And public viewing links can be created with Publish Deck."
  , Tuple
      "img/flipGuide/14.gif"
      "Thanks for exploring your Deck's flip side with us. If you get stuck or want to learn more please check out the help menu."
  ]

dismissedCardGuideKey ∷ String
dismissedCardGuideKey = "dismissedCardGuide"

dismissedFlipGuideKey ∷ String
dismissedFlipGuideKey = "dismissedFlipGuide"
