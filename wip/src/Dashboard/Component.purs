{-
Copyright 2015 SlamData, Inc.

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

module Dashboard.Component
  ( comp
  , toNotebook
  , toDashboard
  , QueryP()
  , StateP()
  , ChildState()
  , ChildSlot()
  , ChildQuery()
  , DialogSlot()
  , NotebookSlot()
  , MenuSlot()
  , module Dashboard.Component.State
  , module Dashboard.Component.Query
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Either (Either())
import Data.Functor (($>))
import Data.Functor.Coproduct
import Data.Lens ((^.))
import Data.Maybe (Maybe(), fromMaybe)
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.HTML.Core (ClassName(), className)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), injSlot, prjSlot, prjQuery)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu
import Halogen.Themes.Bootstrap3 as B

import Render.Common (icon, logo)

import Dashboard.Component.Query
import Dashboard.Component.State
import Dashboard.Menu.Component.Query as Menu
import Dashboard.Menu.Component.State as Menu
import Dashboard.Dialog.Component as Dialog
import Notebook.Common (Slam())
import Notebook.Component as Notebook
import Render.CssClasses as Rc

type DialogSlot = Unit
type NotebookSlot = Unit
data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot = Either DialogSlot (Either NotebookSlot MenuSlot)
type ChildQuery = Coproduct Dialog.QueryP (Coproduct Notebook.NotebookQueryP Menu.QueryP)

type ChildState g = Either Dialog.StateP (Either Notebook.NotebookStateP (Menu.StateP g))

cpDialog :: forall g. ChildPath Dialog.StateP (ChildState g) Dialog.QueryP ChildQuery DialogSlot ChildSlot
cpDialog = cpL

cpNotebook :: forall g. ChildPath Notebook.NotebookStateP (ChildState g) Notebook.NotebookQueryP ChildQuery NotebookSlot ChildSlot
cpNotebook = cpR :> cpL

cpMenu :: forall g. ChildPath (Menu.StateP g) (ChildState g) Menu.QueryP ChildQuery MenuSlot ChildSlot
cpMenu = cpR :> cpR

toDashboard :: (Unit -> Query Unit) -> QueryP Unit
toDashboard = left <<< action

toNotebook :: (Unit -> Notebook.NotebookQuery Unit) -> QueryP Unit
toNotebook = right <<< ChildF (injSlot cpNotebook unit) <<< right <<< left <<< left <<< action

type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type StateP = InstalledState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DashboardHTML = ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DashboardDSL = ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> DashboardHTML
render state =
  H.div
    [ P.classes classes
    , E.onClick (E.input_ DismissAll)
    ]
    [ renderHeader (state ^. _version)
    , H.div
        [ P.classes [ className "sd-menu" ] ]
        [ H.slot' cpMenu MenuSlot \_ ->
          { component: HalogenMenu.menuComponent
          , initialState: installedState Menu.initial
          }
        ]
    , H.slot' cpNotebook unit \_ ->
        { component: Notebook.notebookComponent
        , initialState: Notebook.initialState (state ^. _browserFeatures)
        }
    , H.slot' cpDialog unit \_ ->
        { component: Dialog.comp
        , initialState: Dialog.initialState
        }
    ]

  where
  classes =
    if not (state ^. _editable)
      then [ Rc.notebookViewHack ]
      else [ ]

renderHeader :: Maybe String -> DashboardHTML
renderHeader version =
  H.div_
    [ H.div
        [ P.classes [ B.clearfix ] ]
        [ H.div
            [ P.classes [ Rc.header, B.clearfix ] ]
            [ icon B.glyphiconBook ""
            , logo version
            ]
        ]
    ]

eval :: Natural Query DashboardDSL
eval (Save next) = pure next
eval (DismissAll next) = query' cpMenu MenuSlot (left $ action HalogenMenu.DismissSubmenu) $> next

peek :: forall a. ChildF ChildSlot ChildQuery a -> DashboardDSL Unit
peek (ChildF p q) = coproduct dialogPeek (coproduct notebookPeek menuPeek) q

dialogPeek :: forall a. Dialog.QueryP a -> DashboardDSL Unit
dialogPeek q = pure unit

notebookPeek :: forall a. Notebook.NotebookQueryP a -> DashboardDSL Unit
notebookPeek q = pure unit

menuPeek :: forall a. Menu.QueryP a -> DashboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

submenuPeek :: forall a. (ChildF HalogenMenu.SubmenuSlotAddress (HalogenMenu.SubmenuQuery (Notebook.NotebookQuery Unit))) a -> DashboardDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem q _)) = query' cpNotebook unit (left q) *> pure unit

