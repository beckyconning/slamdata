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

import Utils as Utils
import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils
import Utils.CSS as CSSUtils

data Query a b
  = Selected (ActionInternal a) b
  | UpdateFilter String b
  | UpdateActions (Array (Action a)) b
  | CalculateBoundingRect b
  | SetBoundingElement (Maybe HTMLElement) b

type State a =
  { actions ∷ Array (ActionInternal a)
  , previousActions ∷ Array (ActionInternal a)
  , filterString ∷ String
  , boundingElement ∷ Maybe HTMLElement
  , boundingDimensions ∷ Maybe Dimensions
  }

type HTML a = H.ComponentHTML (Query a)
type DSL a = H.ComponentDSL (State a) (Query a) Slam

-- TODO: Add Pixels newtype
newtype ActionIconSrc = ActionIconSrc String
newtype ActionName = ActionName String
newtype ActionNameWord = ActionNameWord { word ∷ String, widthPx ∷ Number }
newtype ActionDescription = ActionDescription String
newtype ActionHighlighted = ActionHighlighted Boolean

data Action a
  = Do ActionName ActionIconSrc ActionDescription ActionHighlighted a
  | Drill ActionName ActionIconSrc ActionDescription (Array (Action a))

data ActionInternal a
  = DoInternal (Array ActionNameWord) ActionIconSrc ActionDescription ActionHighlighted a
  | DrillInternal (Array ActionNameWord) ActionIconSrc ActionDescription (Array (ActionInternal a))
  | GoBackInternal

data Presentation
  = IconOnly
  | TextOnly
  | IconAndText

type Dimensions = { width ∷ Number, height ∷ Number }

derive newtype instance eqActionIconSrc :: Eq ActionIconSrc
derive newtype instance eqActionDescription :: Eq ActionDescription
derive newtype instance eqActionHighlighted :: Eq ActionHighlighted

instance eqActionNameWord ∷ Eq ActionNameWord where
  eq (ActionNameWord x) (ActionNameWord y) =
    x.word == y.word ∧ x.widthPx == y.widthPx

instance eqActionInternal ∷ Eq a ⇒ Eq (ActionInternal a) where
  eq GoBackInternal GoBackInternal =
    true
  eq (DoInternal n1 i1 d1 h1 a1) (DoInternal n2 i2 d2 h2 a2) =
    n1 ≡ n2
      ∧ i1 ≡ i2
      ∧ d1 ≡ d2
      ∧ h1 ≡ h2
      ∧ a1 ≡ a2
  eq (DrillInternal n1 i1 d1 a1) (DrillInternal n2 i2 d2 a2) =
    n1 ≡ n2
      ∧ i1 ≡ i2
      ∧ d1 ≡ d2
      ∧ a1 ≡ a2
  eq _ _ =
    false

isIconOnly ∷ Presentation → Boolean
isIconOnly =
  case _ of
    IconOnly → true
    _ → false

wordify ∷ ActionName → Array ActionNameWord
wordify (ActionName s) =
  ActionNameWord
    ∘ (\word → { word, widthPx: textWidth word })
    <$> Utils.words s

printActionNameWord ∷ ActionNameWord → String
printActionNameWord (ActionNameWord { word }) =
  word

printActionNameWords ∷ Array ActionNameWord → String
printActionNameWords =
  String.joinWith " " ∘ map printActionNameWord

toActionInternal ∷ ∀ a. Action a → ActionInternal a
toActionInternal =
  case _ of
    Do actionName actionIconSrc actionDescription actionHighlighted a →
      DoInternal
        (wordify actionName)
        actionIconSrc
        actionDescription
        actionHighlighted
        a
    Drill actionName actionIconSrc actionDescription xs →
      DrillInternal
        (wordify actionName)
        actionIconSrc
        actionDescription
        (toActionInternal <$> xs)

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

isDo ∷ ∀ a. ActionInternal a → Boolean
isDo =
  case _ of
    DoInternal _ _ _ _ _ →
      true
    _ →
      false

isDrill ∷ ∀ a. ActionInternal a → Boolean
isDrill =
  case _ of
    DrillInternal _ _ _ _ →
      true
    _ →
      false

isHighlighted ∷ ∀ a. ActionInternal a → Boolean
isHighlighted =
  case _ of
    DoInternal _ _ _ (ActionHighlighted highlighted) _ →
      highlighted
    DrillInternal _ _ _ actions →
      Foldable.any isHighlighted actions
    GoBackInternal → true

searchFilters ∷ ∀ a. ActionInternal a → Array String
searchFilters =
  case _ of
    DoInternal words _ _ _ _ →
      [ printActionNameWords words ]
    DrillInternal words _ _ actions →
      [ printActionNameWords words ] ⊕ Array.concat (map searchFilters actions)
    GoBackInternal →
      [ "go back" ]

actionDescription ∷ ∀ a. ActionInternal a → String
actionDescription =
  case _ of
    DoInternal _ _ (ActionDescription s) _ _ →
      s
    DrillInternal _ _ (ActionDescription s) _ →
      s
    GoBackInternal →
      "Go back"

actionIconSrc ∷ ∀ a. ActionInternal a → String
actionIconSrc =
  case _ of
    DoInternal _ (ActionIconSrc s) _ _ _ →
      s
    DrillInternal _ (ActionIconSrc s) _ _ →
      s
    GoBackInternal →
      "/img/go-back.svg"

actionNameWords ∷ ∀ a. ActionInternal a → Array ActionNameWord
actionNameWords =
  case _ of
    DoInternal xs _ _ _ _ →
      xs
    DrillInternal xs _ _ _ →
      xs
    GoBackInternal →
      wordify $ ActionName "Go back"

initialState ∷ ∀ a. Array (Action a) → State a
initialState actions =
  { actions: toActionInternal <$> actions
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
           renderButtons
           (actionSize
              (Array.length state.actions)
              =<< state.boundingDimensions))
    ]
  where
  iconHeightRatio ∷ Number
  iconHeightRatio =
    0.3

  renderButtons buttonDimensions =
    renderButton presentation buttonDimensions <$> actions
    where
    actions =
      f (buttonDimensions.width * 0.95) <$> state.actions

    maxNumberOfLines =
      fromMaybe 0 $ Foldable.maximum $ Array.length ∘ _.lines <$> actions

    maxTextHeight =
      Int.toNumber maxNumberOfLines * fontSizePx

    -- Allows text which fits vertically but not horizontally.
    -- Styling truncates overflowing lines with ellipses.
    --
    -- E.g. where there is only room for two lines:
    -- "Show Chart" would be presented without an icon as
    --
    -- Sh...
    -- Ch...
    --
    -- But "Build pie chart" would be presented with only an icon.
    --
    -- The reasoning behind this is that presentation should be
    -- unform per action list but that the entire list shouldn't be
    -- reduced to only icons just because "Troubleshoot" would be
    -- truncated to "Troublesh...".
    presentation ∷ Presentation
    presentation =
      if maxTextHeight > buttonDimensions.height ∨ buttonDimensions.width < 40.0
        then
          IconOnly
        else
          if maxTextHeight + (iconHeightRatio + 0.2) * buttonDimensions.height > buttonDimensions.height
            then
              TextOnly
            else
              IconAndText

  f ∷ Number → ActionInternal a → { action ∷ ActionInternal a, lines ∷ Array String }
  f widthPx action =
    { action, lines: lines widthPx $ printActionNameWord <$> actionNameWords action }

  decimalCrop ∷ Int → Number → Number
  decimalCrop i n =
    (Math.floor $ n * multiplier) / multiplier
    where
    multiplier = Math.pow 10.0 $ Int.toNumber i

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

  renderButton
    ∷ Presentation
    → Dimensions
    → { action ∷ ActionInternal a, lines ∷ Array String }
    → HTML a
  renderButton presentation dimensions { action, lines } =
    HH.li
      [ HCSS.style
          $ CSS.width (CSS.px $ firefoxify dimensions.width)
          *> CSS.height (CSS.px $ firefoxify dimensions.height)
      ]
      [ HH.button
          attrs
          $ case presentation of
              IconOnly →
                [ renderIcon 0.5 ]
              TextOnly →
                [ renderName ]
              IconAndText →
                [ renderIcon 0.3, renderName ]
      ]
    where
    renderIcon ∷ Number → HTML a
    renderIcon sizeRatio =
      HH.img
        [ HP.src $ actionIconSrc action
        , HCSS.style
            $ CSS.width (CSS.px $ dimensions.width * sizeRatio)
            *> CSS.height (CSS.px $ dimensions.height * sizeRatio)
            *> CSS.marginBottom (CSS.px $ dimensions.height * 0.05)
            -- Stops icon only presentations from being cut off in squat buttons.
            *> (if (isIconOnly presentation)
                  then
                    CSS.position CSS.absolute
                      *> CSS.left (CSS.px $ iconOnlyLeftPx sizeRatio)
                      *> CSS.top (CSS.px $ iconOnlyTopPx sizeRatio)
                  else
                    CSS.position CSS.relative)
        ]

    iconOnlyLeftPx ∷ Number → Number
    iconOnlyLeftPx sizeRatio =
      (dimensions.width / 2.0) - ((dimensions.width * sizeRatio) / 2.0)

    iconOnlyTopPx ∷ Number → Number
    iconOnlyTopPx sizeRatio =
      (dimensions.height / 2.0) - ((dimensions.height * sizeRatio) / 2.0) - 1.0

    renderName ∷ HTML a
    renderName =
      HH.p
        [ HCSS.style
            $ CSS.fontSize (CSS.px $ fontSizePx)
            *> CSSUtils.lineHeight (show lineHeightPx <> "px")
        ]
        $ Array.intercalate
            [ HH.br_ ]
            $ Array.singleton ∘ HH.text <$> lines

    firefoxify ∷ Number → Number
    firefoxify n =
      if Utils.isFirefox
         then decimalCrop 1 n
         else n

    enabled ∷ Boolean
    enabled =
      case action of
        GoBackInternal →
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
      , HCSS.style $ CSS.position CSS.relative
      ]

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
factors n = do
  factor ← 1 .. n
  guard $ n `mod` factor ≡ 0
  pure factor

nextNonPrime :: Int -> Int
nextNonPrime =
  tailRec go
  where
  go :: Int -> Step Int Int
  go i =
    if Array.length (factors i) ≡ 2
      then Loop $ i + 1
      else Done i

updateActions ∷ ∀ a. Eq a ⇒ Array (ActionInternal a) → State a → State a
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
  activeDrill ∷ Maybe (ActionInternal a)
  activeDrill =
    Foldable.find
      (maybe false (eq state.actions) ∘ pluckDrillActions)
      state.previousActions

  newActiveDrill =
    Foldable.find (eq activeDrill ∘ Just) newActions

  pluckDrillActions =
    case _ of
      DrillInternal _ _ _ xs → Just xs
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
        DoInternal _ _ _ _ _ → pure unit
        DrillInternal _ _ _ actions →
          H.modify
            $ (_actions .~ (GoBackInternal `Array.cons` actions))
            ∘ (_previousActions .~ st.actions)
        GoBackInternal →
          H.modify
            $ (_actions .~ st.previousActions)
            ∘ (_previousActions .~ [ ])
      pure next
    UpdateActions actions next →
      H.modify (updateActions $ toActionInternal <$> actions) $> next
    CalculateBoundingRect next →
      (H.modify
        ∘ (_boundingDimensions .~ _)
        ∘ map domRectToDimensions
        =<< getBoundingDOMRect)
        $> next
    SetBoundingElement element next →
      H.modify (_boundingElement .~ element)
        $> next

floor ∷ Dimensions → Dimensions
floor dimensions =
  { width: Math.floor dimensions.width
  , height: Math.floor dimensions.height
}

lines ∷ Number → Array String → Array String
lines width words =
  foldl go [] words
  where
  go ∷ Array String → String → Array String
  go acc s =
    case Array.uncons acc of
      Nothing →
        [ s ]
      Just { head, tail } →
        if (textWidth $ head <> " " <> s) <= width
          then Array.snoc tail (head <> " " <> s)
          else Array.snoc acc s

textWidth ∷ String → Number
textWidth =
  flip
    DOMUtils.getTextWidthPure
    $ "normal " <> show fontSizePx <> "px Ubuntu"

fontSizePx ∷ Number
fontSizePx =
  12.0

lineHeightPx ∷ Number
lineHeightPx =
  13.0

iconSizePercent ∷ Number
iconSizePercent =
  30.0

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
