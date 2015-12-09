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

import DOM.Event.EventTarget (EventListener())
import Data.BrowserFeatures (BrowserFeatures())
import Data.KeyCombination (KeyCombination())
import Data.Lens (LensP(), lens)
import Data.Maybe (Maybe(..))
import Data.ModifierKey (ModifierKey(..))
import Data.NonModifierKey (NonModifierKey(..))
import Data.Path.Pathy (rootDir)
import Model.AccessType (AccessType(..))
import Model.CellId (CellId())
import Model.CellType (CellType(..))
import Notebook.Component as Notebook
import Notebook.Effects (NotebookEffects())
import Utils.Path (DirPath())

import Dashboard.Menu.Component.Query (Value(), notebookQueryToValue)

type KeyboardShortcut = { combination :: KeyCombination, value :: Value, label :: Maybe String }

type State =
  { accessType :: AccessType
  , browserFeatures :: BrowserFeatures
  , keyboardShortcuts :: Array KeyboardShortcut
  , keyboardListeners :: Array (EventListener NotebookEffects)
  , loaded :: Boolean
  , path :: DirPath
  , viewingCell :: Maybe CellId
  , version :: Maybe String
  }

keyboardShortcuts :: Array KeyboardShortcut
keyboardShortcuts =
  [ { combination: { modifiers: [Control], key: Digit1 }
    , value: notebookQueryToValue $ (Notebook.AddCell Query) unit
    , label: Nothing
    }
  , { combination: { modifiers: [Control], key: Digit2 }
    , value: notebookQueryToValue $ (Notebook.AddCell Markdown) unit
    , label: Nothing
    }
  , { combination: { modifiers: [Control], key: Digit3 }
    , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
    , label: Nothing
    }
  , { combination: { modifiers: [Control], key: Digit4 }
    , value: notebookQueryToValue $ (Notebook.AddCell Search) unit
    , label: Nothing
    }
  , { combination: { modifiers: [Control], key: Enter }
    , value: notebookQueryToValue $ (Notebook.RunActiveCell) unit
    , label: Nothing
    }
  ]

initialState :: { browserFeatures :: BrowserFeatures } -> State
initialState rec =
  { accessType: Editable
  , browserFeatures: rec.browserFeatures
  , keyboardShortcuts: keyboardShortcuts
  , keyboardListeners: []
  , loaded: false
  , path: rootDir
  , viewingCell: Nothing
  , version: Nothing
  }

_accessType :: LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_keyboardShortcuts :: LensP State (Array KeyboardShortcut)
_keyboardShortcuts = lens _.keyboardShortcuts _{keyboardShortcuts = _}

_keyboardListeners :: LensP State (Array (EventListener NotebookEffects))
_keyboardListeners = lens _.keyboardListeners _{keyboardListeners = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_path :: LensP State DirPath
_path = lens _.path _{path = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

_version :: LensP State (Maybe String)
_version = lens _.version _{version = _}
