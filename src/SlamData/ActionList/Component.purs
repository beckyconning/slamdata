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

module SlamData.ActionList.Component where

import SlamData.Prelude

import Control.Monad.Rec.Class (tailRec, Step(Loop, Done))

import CSS as CSS

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Lens (Lens', lens, (.~))
import Data.String as String

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import Math as Math

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC

import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils

data Query a b
  = Selected (Action a) b
  | UpdateFilter String b
  | UpdateActions (Array (Action a)) b
  | CalculateBoundingRect b
  | SetBoundingElement (Maybe HTMLElement) b

type State a =
  { actions ∷ Array (Action a)
  , previousActions ∷ Array (Action a)
  , filterString ∷ String
  , boundingElement ∷ Maybe HTMLElement
  , boundingDimensions ∷ Maybe Dimensions
  }

type HTML a = H.ComponentHTML (Query a)
type DSL a = H.ComponentDSL (State a) (Query a) Slam

newtype ActionIconSrc = ActionIconSrc String
newtype ActionName = ActionName String
newtype ActionDescription = ActionDescription String
newtype ActionHighlighted = ActionHighlighted Boolean

data Action a
  = Do ActionName ActionIconSrc ActionDescription ActionHighlighted a
  | Drill ActionName ActionIconSrc ActionDescription (Array (Action a))
  | GoBack

type Dimensions = { width ∷ Number, height ∷ Number }

_actions ∷ ∀ a r. Lens' { actions ∷ a |r } a
_actions =
  lens _.actions (_ { actions = _ })

_previousActions ∷ ∀ a r. Lens' { previousActions ∷ a | r} a
_previousActions =
  lens _.previousActions (_ { previousActions = _ })

_filterString ∷ ∀ a r. Lens' { filterString ∷ a | r } a
_filterString =
  lens _.filterString (_ { filterString = _ })

_boundingElement ∷ ∀ a r. Lens' { boundingElement ∷ a | r } a
_boundingElement =
  lens _.boundingElement (_ { boundingElement = _ })

_boundingDimensions ∷ ∀ a r. Lens' { boundingDimensions ∷ a | r } a
_boundingDimensions =
  lens _.boundingDimensions (_ { boundingDimensions = _ })

isDo ∷ ∀ a. Action a → Boolean
isDo =
  case _ of
    Do _ _ _ _ _ →
      true
    _ →
      false

isDrill ∷ ∀ a. Action a → Boolean
isDrill =
  case _ of
    Drill _ _ _ _ →
      true
    _ →
      false

isHighlighted ∷ ∀ a. Action a → Boolean
isHighlighted =
  case _ of
    Do _ _ _ (ActionHighlighted highlighted) _ →
      highlighted
    Drill _ _ _ actions →
      Foldable.any isHighlighted actions
    GoBack → true

searchFilters ∷ ∀ a. Action a → Array String
searchFilters =
  case _ of
    Do (ActionName name) _ _ _ _ →
      [ name ]
    Drill (ActionName name) _ _ actions →
      [ name ] ⊕ Array.concat (map searchFilters actions)
    GoBack →
      [ "go back" ]

derive newtype instance eqActionIconSrc :: Eq ActionIconSrc
derive newtype instance eqActionName :: Eq ActionName
derive newtype instance eqActionDescription :: Eq ActionDescription
derive newtype instance eqActionHighlighted :: Eq ActionHighlighted

instance eqAction ∷ Eq a ⇒ Eq (Action a) where
  eq GoBack GoBack =
    true
  eq (Do n1 i1 d1 h1 a1) (Do n2 i2 d2 h2 a2) =
    n1 ≡ n2
      ∧ i1 ≡ i2
      ∧ d1 ≡ d2
      ∧ h1 ≡ h2
      ∧ a1 ≡ a2
  eq (Drill n1 i1 d1 a1) (Drill n2 i2 d2 a2) =
    n1 ≡ n2
      ∧ i1 ≡ i2
      ∧ d1 ≡ d2
      ∧ a1 ≡ a2
  eq _ _ =
    false

initialState ∷ ∀ a. Array (Action a) → State a
initialState actions =
  { actions
  , previousActions: [ ]
  , filterString: ""
  , boundingElement: Nothing
  , boundingDimensions: Nothing
  }

comp ∷ ∀ a. Eq a ⇒ H.Component (State a) (Query a) Slam
comp =
  H.lifecycleComponent
    { render
    , initializer: Just $ H.action CalculateBoundingRect
    , finalizer: Nothing
    , eval
    }

render ∷ ∀ a. State a → HTML a
render state =
  HH.div
    [ HP.class_ $ HH.className "sd-action-list" ]
    [ HH.form_
        [ HH.div_
            [ HH.div
                [ HP.class_ (HH.className "sd-action-filter-icon") ]
                [ RC.searchFieldIcon ]
            , HH.input
                [ HP.value state.filterString
                , HE.onValueInput (HE.input (\s → UpdateFilter s))
                , ARIA.label "Filter next actions"
                , HP.placeholder "Filter actions"
                ]
            , HH.button
                [ HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ (UpdateFilter ""))
                , HP.enabled (state.filterString /= "")
                ]
                [ RC.clearFieldIcon "Clear filter" ]
            ]
        ]
    , HH.ul
        [ HP.ref $ H.action ∘ SetBoundingElement ]
        (maybe
           []
           (\buttonDimensions → button buttonDimensions <$> state.actions)
           (flipIfOneRow
              <$> state.boundingDimensions
              <*> (actionSize
                    (Array.length state.actions)
                    =<< state.boundingDimensions)))
    ]
  where
  actionSize ∷ Int → Dimensions → Maybe Dimensions
  actionSize i boundingDimensions = do
    firstTry ← mostSquareFittingRectangle i boundingDimensions
    if firstTry.height ≡ boundingDimensions.height
      then do
        secondTry ← mostSquareFittingRectangle (nextNonPrime i) boundingDimensions
        if secondTry.height ≡ boundingDimensions.height
           then pure firstTry
           else pure secondTry
      else pure firstTry


  filterString ∷ String
  filterString =
    String.toLower state.filterString

  button ∷ Dimensions → Action a → HTML a
  button dimensions action =
    HH.li
      [ HCSS.style
          $ CSS.width (CSS.px dimensions.width)
          *> CSS.height (CSS.px dimensions.height)
      ]
      [ HH.button attrs
          [ HH.img [ HP.src $ actionIconSrc action ]
          , HH.p_ [ HH.text $ actionName action ]
          ]
      ]
    where
    enabled ∷ Boolean
    enabled =
      case action of
        GoBack →
          true
        _ →
          Foldable.any
            (String.contains (String.Pattern filterString) ∘ String.toLower)
            (searchFilters action)

    attrs =
      [ HP.title $ actionDescription action
      , HP.disabled $ not enabled
      , ARIA.label $ actionDescription action
      , HP.classes classes
      , HE.onClick (HE.input_ $ Selected action)
      , HP.buttonType HP.ButtonButton
      ]

    actionDescription ∷ Action a → String
    actionDescription =
      case _ of
        Do _ _ (ActionDescription s) _ _ →
          s
        Drill _ _ (ActionDescription s) _ →
          s
        GoBack →
          "Go back"

    actionIconSrc ∷ Action a → String
    actionIconSrc =
      case _ of
        Do _ (ActionIconSrc s) _ _ _ →
          s
        Drill _ (ActionIconSrc s) _ _ →
          s
        GoBack →
          "/img/go-back.svg"

    actionName ∷ Action a → String
    actionName =
      case _ of
        Do (ActionName s) _ _ _ _ →
          s
        Drill (ActionName s) _ _ _ →
          s
        GoBack →
          "Go back"

    classes ∷ Array HH.ClassName
    classes =
      if isHighlighted action && enabled
        then
          [ HH.className "sd-button" ]
        else
          [ HH.className "sd-button"
          , HH.className "sd-button-warning"
          ]

factors ∷ Int → Array Int
factors n = spy $ do
  factor ← 1 .. n
  traceAnyA factor
  guard $ n `mod` factor ≡ 0
  pure factor

nextNonPrime :: Int -> Int
nextNonPrime = tailRec go
  where
  go :: Int -> Step Int Int
  go i =
    if Array.length (factors i) ≡ 2
      then Loop $ i + 1
      else Done i

updateActions ∷ ∀ a. Eq a ⇒ Array (Action a) → State a → State a
updateActions newActions state =
  case activeDrill of
    Nothing →
      state
        { actions = newActions }
    Just drill →
      state
        { previousActions = newActions
        , actions = fromMaybe [] $ pluckDrillActions =<< newActiveDrill
        }
  where
  activeDrill ∷ Maybe (Action a)
  activeDrill =
    Foldable.find
      (maybe false (eq state.actions) ∘ pluckDrillActions)
      state.previousActions

  newActiveDrill =
    Foldable.find (eq activeDrill ∘ Just) newActions

  pluckDrillActions =
    case _ of
      Drill _ _ _ xs → Just xs
      _ → Nothing

domRectToDimensions ∷ DOMRect → Dimensions
domRectToDimensions domRect =
  { width: domRect.width, height: domRect.height }

getBoundingDOMRect ∷ ∀ a. DSL a (Maybe DOMRect)
getBoundingDOMRect =
  traverse (H.fromEff ∘ DOMUtils.getOffsetClientRect)
    =<< H.gets _.boundingElement

eval ∷ ∀ a. Eq a ⇒ Query a ~> DSL a
eval =
  case _ of
    UpdateFilter str next →
      H.modify (_filterString .~ str) $> next
    Selected action next → do
      st ← H.get
      case action of
        Do _ _ _ _ _ → pure unit
        Drill _ _ _ actions →
          H.modify
            $ (_actions .~ (GoBack `Array.cons` actions))
            ∘ (_previousActions .~ st.actions)
        GoBack →
          H.modify
            $ (_actions .~ st.previousActions)
            ∘ (_previousActions .~ [ ])
      pure next
    UpdateActions actions next →
      H.modify (updateActions actions) $> next
    CalculateBoundingRect next →
      (H.modify
        ∘ (_boundingDimensions .~ _)
        ∘ map domRectToDimensions
        =<< getBoundingDOMRect)
        $> next
    SetBoundingElement element next →
      H.modify (_boundingElement .~ element)
        $> next

flipIfOneRow ∷ Dimensions → Dimensions → Dimensions
flipIfOneRow boundingDimensions dimensions =
  if dimensions.height ≡ boundingDimensions.height
    then { width: boundingDimensions.width, height: rowHeight }
    else dimensions
  where
  numberOfColumns = boundingDimensions.width / dimensions.width
  rowHeight = boundingDimensions.height / numberOfColumns

mostSquareFittingRectangle ∷ Int → Dimensions → Maybe Dimensions
mostSquareFittingRectangle i boundingDimensions =
  Foldable.maximumBy
    (\x y → karat x `compare` karat y)
    solutions
  where
  solutions ∷ Array Dimensions
  solutions =
    solution <$> factors i

  goldenRatio ∷ Number
  goldenRatio =
    1.61803398875

  karat ∷ Dimensions → Number
  karat dimensions =
    -(Math.abs $ goldenRatio - (dimensions.width / dimensions.height))

  solution ∷ Int → Dimensions
  solution factor =
    { width: boundingDimensions.width / (Int.toNumber $ numberOfRows factor)
    , height: boundingDimensions.height / (Int.toNumber $ numberOfColumns factor)
    }

  numberOfRows ∷ Int → Int
  numberOfRows factor =
    if boundingDimensions.width > boundingDimensions.height
       then factor
       else i / factor

  numberOfColumns ∷ Int → Int
  numberOfColumns factor =
    if boundingDimensions.width > boundingDimensions.height
       then i / factor
       else factor
