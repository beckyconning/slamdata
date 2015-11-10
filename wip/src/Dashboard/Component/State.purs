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

module Dashboard.Component.State where

import Prelude

import Data.Lens (LensP(), lens)
import Data.BrowserFeatures (BrowserFeatures())
import Data.Maybe (Maybe(..))

type StateRec =
  { editable :: Boolean
  , browserFeatures :: BrowserFeatures
  , loaded :: Boolean
  , version :: Maybe String
  }

newtype State = State StateRec

initialState :: BrowserFeatures -> State
initialState fs =
  State { editable: true
        , browserFeatures: fs
        , loaded: false
        , version: Nothing
        }

_State :: LensP State StateRec
_State = lens (\(State obj) -> obj) (const State)

_editable :: LensP State Boolean
_editable = _State <<< lens _.editable _{editable = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = _State <<< lens _.browserFeatures _{browserFeatures = _}

_loaded :: LensP State Boolean
_loaded = _State <<< lens _.loaded _{loaded = _}

_version :: LensP State (Maybe String)
_version = _State <<< lens _.version _{version = _}

