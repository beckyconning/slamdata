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

module SlamData.Workspace.Card.Open.Component.State
  ( State
  , StateP
  , initialState
  , _selected
  , _levelOfDetails
  , _loading
  ) where

import SlamData.Prelude

import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.Open.Component.Query (QueryC)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Monad (Slam)

import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI

import Utils.Path (AnyPath)

type State =
  { selected ∷ Maybe R.Resource
  , levelOfDetails ∷ LevelOfDetails
  , loading ∷ Boolean
  }

initialState ∷ State
initialState =
  { selected: Nothing
  , levelOfDetails: High
  , loading: false
  }

_selected ∷ ∀ a r. Lens' { selected ∷ a|r} a
_selected = lens (_.selected) (_{selected = _})

_levelOfDetails ∷ ∀ a r. Lens' {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

_loading ∷ ∀ a r. Lens' {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

type StateP =
  ParentState
    State
    (MCI.BasicColumnsState R.Resource AnyPath)
    (QueryC)
    (MCI.BasicColumnsQuery R.Resource AnyPath)
    Slam
    Unit
