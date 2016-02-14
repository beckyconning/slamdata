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

module SlamData.SignIn.Component
  ( comp
  , MenuSlot()
  , QueryP()
  , StateP()
  , ChildQuery()
  , Query(..)
  , ChildSlot()
  , ChildState()
  , module SlamData.SignIn.Component.State
  ) where

import Prelude

import Control.Apply ((*>))
--import Control.Bind ((>=>), (=<<))
--import Control.UI.Browser (newTab, locationString)

import Data.Const (Const(), getConst)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Functor.Eff (liftEff)
--import Data.Functor.Aff (liftAff)
import Data.Generic (Generic, gEq, gCompare)
import Data.Void (Void(), absurd)

import Halogen
import Halogen.HTML.Core (className)
--import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu
--import Halogen.Themes.Bootstrap3 as B

import OIDC.Aff (requestAuthentication)
import SlamData.Effects (Slam())
import SlamData.SignIn.Component.State
--import SlamData.Render.CSS as Rc
import SlamData.SignIn.Menu.Component.Query as Menu
import SlamData.SignIn.Menu.Component.State as Menu
import Quasar.Auth.Provider as Provider

--import Utils.DOM (documentTarget)

data Query a = DismissSubmenu a

type QueryP = Coproduct Query (ChildF MenuSlot ChildQuery)

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot = MenuSlot

type ChildQuery = Menu.QueryP

type ChildState g = Menu.StateP g

type StateP = InstalledState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardHTML = ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardDSL = ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> DraftboardHTML
render state =
  H.div
    [ P.classes $ [ className "sd-sign-in" ] ]
    [ H.slot MenuSlot \_ ->
      { component: HalogenMenu.menuComponent
      , initialState: installedState $ Menu.make []
      }
    ]

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce "hole"

eval :: Natural Query DraftboardDSL
eval (DismissSubmenu next) = dismissAll $> next

dismissAll :: DraftboardDSL Unit
dismissAll =
  queryMenu $
    action HalogenMenu.DismissSubmenu

makeAuthRequestWithProviderR :: Provider.ProviderR -> DraftboardDSL Unit
makeAuthRequestWithProviderR _ = liftEff requestAuthentication *> pure unit

peek :: forall a. ChildF ChildSlot ChildQuery a -> DraftboardDSL Unit
peek (ChildF p q) = menuPeek q

menuPeek :: forall a. Menu.QueryP a -> DraftboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

evaluateMenuValue :: Provider.ProviderR -> DraftboardDSL Unit
evaluateMenuValue _ = pure unit -- do the auth request

submenuPeek
  :: forall a
   . ChildF HalogenMenu.SubmenuSlotAddress (HalogenMenu.SubmenuQuery Provider.ProviderR) a
  -> DraftboardDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem v _)) = makeAuthRequestWithProviderR v

queryMenu :: HalogenMenu.MenuQuery Provider.ProviderR Unit -> DraftboardDSL Unit
queryMenu q = query MenuSlot (left q) *> pure unit
