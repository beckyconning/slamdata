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
  , fromNotebook
  , toDashboard
  , fromDashboard
  , QueryP()
  , StateP()
  , ChildState()
  , ChildSlot()
  , ChildQuery()
  , DialogSlot()
  , MenuSlot()
  , NotebookSlot()
  , module Dashboard.Component.State
  , module Dashboard.Component.Query
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Apply ((*>))
import Data.Either (Either(), either)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Generic (Generic, gEq, gCompare)
import Data.Lens ((^.), (.~))
import Data.Maybe (Maybe(), fromMaybe)
import DOM (DOM())
import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), injSlot, prjSlot, prjQuery)
import Halogen.HTML.Core (ClassName(), className)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu
import Halogen.Themes.Bootstrap3 as B

import Dashboard.Component.Query
import Dashboard.Component.State
import Dashboard.Dialog.Component as Dialog
import Dashboard.Menu.Component.State as Menu
import Dashboard.Menu.Component.Query as Menu
import Model.AccessType (isReadOnly)
import Notebook.Common (Slam())
import Notebook.Component as Notebook
import Render.CssClasses as Rc
import Render.Common (icon, logo)

type DialogSlot = Unit
type NotebookSlot = Unit
data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot = Either MenuSlot (Either DialogSlot NotebookSlot)
type ChildQuery = Coproduct Menu.QueryP (Coproduct Dialog.QueryP Notebook.NotebookQueryP)
type ChildState g = Either (Menu.StateP g) (Either Dialog.StateP Notebook.NotebookStateP)

cpDialog :: forall g. ChildPath Dialog.StateP (ChildState g) Dialog.QueryP ChildQuery DialogSlot ChildSlot
cpDialog = cpR :> cpL

cpNotebook :: forall g. ChildPath Notebook.NotebookStateP (ChildState g) Notebook.NotebookQueryP ChildQuery NotebookSlot ChildSlot
cpNotebook = cpR :> cpR

cpMenu :: forall g. ChildPath (Menu.StateP g) (ChildState g) Menu.QueryP ChildQuery MenuSlot ChildSlot
cpMenu = cpL

toDashboard :: (Unit -> Query Unit) -> QueryP Unit
toDashboard = left <<< action

fromDashboard
  :: forall a. (forall i. (a -> i) -> Query i) -> QueryP a
fromDashboard r = left $ request r

toNotebook :: (Unit -> Notebook.NotebookQuery Unit) -> QueryP Unit
toNotebook =
  right <<< ChildF (injSlot cpNotebook unit)
  <<< right
  <<< right
  <<< left
  <<< action

fromNotebook
  :: forall a. (forall i. (a -> i) -> Notebook.NotebookQuery i) -> QueryP a
fromNotebook r =
  right
  $ ChildF (injSlot cpNotebook unit)
  $ right
  $ right
  $ left
  $ request r

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
    [ H.nav_
        [ renderHeader (state ^. _version) ]
    , H.div
        [ P.classes [ className "sd-menu" ] ]
        [ H.slot' cpMenu MenuSlot \_ ->
          { component: HalogenMenu.menuComponent
          , initialState: installedState Menu.initial
          }
        ]
    , H.div
        [ P.classes [ className "sd-notebook" ] ]
        [  H.slot' cpNotebook unit \_ ->
          { component: Notebook.notebookComponent
          , initialState: Notebook.initialState (state ^. _browserFeatures)
          }
        ]
    , H.slot' cpDialog unit \_ ->
        { component: Dialog.comp
        , initialState: installedState Dialog.initialState
        }
    ]

  where
  classes = if isReadOnly (state ^. _accessType)
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
eval (GetPath continue) = map continue $ gets _.path
eval (SetAccessType aType next) = do
  modify (_accessType .~ aType)
  query' cpNotebook unit
    $ left $ action $ Notebook.SetAccessType aType
  pure next
eval (SetViewingCell mbcid next) = do
  modify (_viewingCell .~ mbcid)
  query' cpNotebook unit
    $ left $ action $ Notebook.SetViewingCell mbcid
  pure next
eval (DismissAll next) =
  query' cpMenu MenuSlot (left $ action HalogenMenu.DismissSubmenu) $> next

peek :: forall a. ChildF ChildSlot ChildQuery a -> DashboardDSL Unit
peek (ChildF p q) = coproduct menuPeek (coproduct dialogPeek notebookPeek) q

dialogPeek :: forall a. Dialog.QueryP a -> DashboardDSL Unit
dialogPeek q = pure unit

notebookPeek :: forall a. Notebook.NotebookQueryP a -> DashboardDSL Unit
notebookPeek q = pure unit

menuPeek :: forall a. Menu.QueryP a -> DashboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

submenuPeek :: forall a. (ChildF HalogenMenu.SubmenuSlotAddress (HalogenMenu.SubmenuQuery Menu.Value)) a -> DashboardDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem v _)) =
  either presentHelp (coproduct queryDialog queryNotebook) v

queryDialog :: Dialog.Query Unit -> DashboardDSL Unit
queryDialog q = query' cpDialog unit (left q) *> pure unit

queryNotebook :: Notebook.NotebookQuery Unit -> DashboardDSL Unit
queryNotebook q = query' cpNotebook unit (left q) *> pure unit

presentHelp :: Menu.HelpURI -> DashboardDSL Unit
presentHelp (Menu.HelpURI uri) = liftH $ liftEff' $ newTab uri

foreign import newTab :: forall e. String -> Eff (dom :: DOM | e) Unit
