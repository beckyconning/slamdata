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

module SlamData.Workspace.Card.Pending.Component.State
  ( State
  , initialState
  , _message
  ) where

import Data.Lens (Lens', lens)

type State =
  { message ∷ String
  }

initialState ∷ State
initialState =
  { message: "Please wait while the deck is evaluated"
  }

_message ∷ Lens' State String
_message = lens (_.message) (_ { message = _ })
