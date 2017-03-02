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

module SlamData.Workspace.Card.Tabs.Component.Query where

import Halogen as H
import Halogen.Component.Utils.Drag (DragEvent)

import SlamData.Workspace.Eval.Deck as Deck

import Utils.DOM as DOM

data Query a
  = AddTab a
  | HandleMessage Deck.Id Deck.EvalMessage (H.SubscribeStatus → a)
  | OrderStart Int DOM.MouseEvent a
  | Ordering Int DragEvent a
  | OrderOver Int a
  | OrderOut Int a
