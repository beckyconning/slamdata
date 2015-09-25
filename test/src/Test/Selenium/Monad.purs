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

module Test.Selenium.Monad where

import Prelude
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (maybe, fromMaybe)
import Data.List (length)
import Data.List.Unsafe (head)
import Platform (getPlatform, PLATFORM(), runOs, runPlatform)
import Selenium.Monad (Selenium(), byCss, byXPath, findElements)
import Selenium (showLocator)
import Selenium.Types (ControlKey(), Locator(), Element())
import Selenium.Key (metaKey, controlKey)
import Test.Config (Config())
import qualified Graphics.ImageDiff as GI
import qualified Graphics.EasyImage as GE
import Node.FS (FS())

type Check a = Selenium ( platform :: PLATFORM
                        , imageDiff :: GI.IMAGE_MAGICK
                        , easyImage :: GE.EASY_IMAGE
                        , fs :: FS) (config :: Config) a

getConfig :: Check Config
getConfig = _.config <$> ask

getPlatformString :: Check String
getPlatformString = do
  platform <- getPlatform
  pure $ fromMaybe ""
    $ platform
    >>= runPlatform
    >>> _.os
    >>> runOs
    >>> _.family

findSingle :: Locator -> Check Element
findSingle locator = do
  elements <- findElements locator
  case length elements of
    1 -> return $ head elements
    0 -> throwError $ error $ "Couldn't find an element with the locator: " ++ showLocator locator
    _ -> throwError $ error $ "Found more than one element with the locator: " ++ showLocator locator

getModifierKey :: Check ControlKey
getModifierKey = map modifierKey getPlatformString
  where
  modifierKey "Darwin" = metaKey
  modifierKey _ = controlKey

diff :: _ -> Check Boolean
diff = liftAff <<< GI.diff

byAriaLabel :: String -> Check Locator
byAriaLabel label = byCss $ "*[aria-label='" ++ label ++ "']"

byExactText :: String -> Check Locator
byExactText exactText = byXPath $ "//*[text()='" ++ exactText ++ "']"

byText :: String -> Check Locator
byText text = byXPath $ "//*[contains(., '" ++ text ++ "')]"
