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

module Utils.LocalStorage
  ( onStorageEvent
  , StorageEvent
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Foreign (Foreign)
import Unsafe.Coerce as U

import DOM (DOM)
import DOM.Event.Types (Event, EventType(..))
import DOM.Event.EventTarget as EventTarget
import DOM.HTML as DOMHTML
import DOM.HTML.Types as DOMHTMLTypes

type StorageEvent =
  { key ∷ String
  , oldValue ∷ String
  , newValue ∷ String
  , url ∷ String
  , storageArea ∷ Foreign
  }

eventToStorageEvent ∷ Event → StorageEvent
eventToStorageEvent = U.unsafeCoerce

onStorageEvent
  ∷ ∀ eff
  . (StorageEvent → Eff (dom ∷ DOM | eff) Unit)
  → Eff (dom ∷ DOM | eff) (Eff (dom ∷ DOM | eff) Unit)
onStorageEvent cb = do
  win ← DOMHTMLTypes.windowToEventTarget <$> DOMHTML.window
  let listener = EventTarget.eventListener (cb <<< eventToStorageEvent)
  EventTarget.addEventListener (EventType "storage") listener false win
  pure $ EventTarget.removeEventListener (EventType "storage") listener false win
