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

import Data.BrowserFeatures (BrowserFeatures())
import Data.Key (Platform())
import Data.Lens (LensP(), lens)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Model.AccessType (AccessType(..))
import Model.CellId (CellId())
import Utils.Path (DirPath())

type State =
  { accessType :: AccessType
  , browserFeatures :: BrowserFeatures
  , platform :: Platform
  , loaded :: Boolean
  , path :: DirPath
  , viewingCell :: Maybe CellId
  , version :: Maybe String
  }

initialState :: { browserFeatures :: BrowserFeatures, platform :: Platform } -> State
initialState rec =
  { accessType: Editable
  , browserFeatures: rec.browserFeatures
  , platform: rec.platform
  , loaded: false
  , path: rootDir
  , viewingCell: Nothing
  , version: Nothing
  }

_accessType :: LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_platform :: LensP State Platform
_platform = lens _.platform _{platform = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_path :: LensP State DirPath
_path = lens _.path _{path = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

_version :: LensP State (Maybe String)
_version = lens _.version _{version = _}
