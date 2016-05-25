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

module Utils.DOM where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (EventTarget)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlElementToParentNode, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode as P
import DOM.Node.Types (Element, documentToEventTarget)
import Data.Nullable (toMaybe)
import SlamData.Prelude
import Unsafe.Coerce (unsafeCoerce)

foreign import waitLoaded ∷ forall e. Aff (dom ∷ DOM |e) Unit
foreign import onLoad ∷ forall e. Eff e Unit → Eff e Unit
foreign import blur ∷ forall e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import focus ∷ forall e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import offsetLeft ∷ forall e. HTMLElement → Eff (dom ∷ DOM|e) Number
foreign import getBoundingClientRect ∷ forall eff.  HTMLElement → Eff (dom ∷ DOM | eff) DOMRect
foreign import getTextWidth ∷ forall eff. String → String → Eff (dom ∷ DOM | eff) Number
foreign import getRootElementFontSizePx ∷ forall eff. Eff (dom ∷ DOM | eff) Number

type DOMRect =
  { left ∷ Number
  , top ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

elementToHTMLElement ∷ Element → HTMLElement
elementToHTMLElement =
  unsafeCoerce

querySelector ∷ forall e. String → HTMLElement → Eff (dom ∷ DOM|e) (Maybe HTMLElement)
querySelector s =
  map (map elementToHTMLElement <<< toMaybe) <<< P.querySelector s <<< htmlElementToParentNode

documentTarget ∷ forall e. Eff (dom ∷ DOM|e) EventTarget
documentTarget =
  htmlDocumentToEventTarget <$> (document =<< window)
  where
  htmlDocumentToEventTarget = documentToEventTarget <<< htmlDocumentToDocument

querySelectorBoundingClientRect ∷ forall e. String → HTMLElement → Eff (dom ∷ DOM|e) (Maybe DOMRect)
querySelectorBoundingClientRect s =
  traverse getBoundingClientRect <=< querySelector s

remToPx ∷ forall e. Number → Eff (dom ∷ DOM|e) Number
remToPx n =
  map (_ * n) getRootElementFontSizePx
