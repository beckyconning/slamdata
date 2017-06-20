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

module Test.Feature.ActionSequence
  ( click
  , sendDelete
  , sendEnter
  , shifted
  , keys
  , sendBackspaces
  , sendRights
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.List (List)
import Data.String as S
import Data.Unfoldable (replicate)

import Selenium.ActionSequence (Sequence, sendKeys, keyDown, keyUp, mouseDown, mouseUp)
import Selenium.Key (shiftKey)
import Selenium.MouseButton (leftButton)
import Selenium.Types (ControlKey, Element)

click ∷ Element → Sequence Unit
click element = do
  mouseDown leftButton element
  mouseUp leftButton element

shifted ∷ String → Sequence Unit
shifted str = do
  keyDown shiftKey
  sendKeys str
  keyUp shiftKey

sendDelete ∷ Sequence Unit
sendDelete = sendKeys "\xE017"

sendBackspaces ∷ Int → Sequence Unit
sendBackspaces n = traverse_ sendKeys $ replicate n "\xE003" :: List String

sendRights ∷ Int → Sequence Unit
sendRights n = traverse_ sendKeys $ replicate n "\xE014" :: List String

sendEnter ∷ Sequence Unit
sendEnter = sendKeys "\xE007"

sendKeyCombo ∷ Array ControlKey → String → Sequence Unit
sendKeyCombo ctrlKeys str = do
  traverse_ keyDown ctrlKeys
  sendKeys str
  traverse_ keyUp ctrlKeys

-- Send keys one by one.
keys ∷ String → Sequence Unit
keys str = traverse_ sendKeys $ S.split (S.Pattern "") str
