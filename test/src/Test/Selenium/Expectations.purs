module Test.Selenium.Expectations where

import Prelude

import Selenium.Combinators (tryToFind)
import Selenium.Monad (getAttribute, childExact, byXPath, byId, tryRepeatedlyTo)
import Data.Maybe (Maybe(..))
import Test.Selenium.Monad (Check())
import Test.Selenium.Finders (findSingle, findElementIdByLabelText)
import Test.Selenium.Expect (expect, toEq)
import Test.Selenium.Locators (checkableLocator)

import Data.Traversable (traverse) as T

expectDropdownWithLabelOptionsAndValue :: String -> Array String -> String -> Check Unit
expectDropdownWithLabelOptionsAndValue expectedLabel expectedOptions expectedValue = tryRepeatedlyTo do
  selectId <- findElementIdByLabelText expectedLabel
  select <- tryToFind $ byId selectId
  value <- getAttribute select "value"
  expect value toEq $ Just expectedValue
  void $ T.traverse (findOption select) expectedOptions
    where
    optionXPath text = "//option[text()=\"" ++ text ++ "\"]"
    findOption select text = byXPath (optionXPath text) >>= childExact select

expectInputWithLabelTypeAndValue :: String -> String -> String -> Check Unit
expectInputWithLabelTypeAndValue expectedLabel expectedInputType expectedValue = tryRepeatedlyTo do
  inputId <- findElementIdByLabelText expectedLabel
  input <- tryToFind $ byId inputId
  value <- getAttribute input "value"
  inputType <- getAttribute input "type"
  expect value toEq $ Just expectedValue
  expect inputType toEq $ Just expectedInputType

expectLabel :: String -> Check Unit
expectLabel expected = void $ tryRepeatedlyTo $ byXPath labelXPath >>= findSingle
  where
  labelXPath = "//label[text()=\"" ++ expected ++ "\"]"

expectInputWithLabelTypeAndChecked :: String -> String -> Boolean -> Check Unit
expectInputWithLabelTypeAndChecked expectedLabel expectedType expectedChecked = tryRepeatedlyTo do
  inputId <- findElementIdByLabelText expectedLabel
  void $ tryToFind $ checkableLocator expectedType expectedChecked inputId

--hideQueryCellOptions :: Check Unit
--hideQueryCellOptions = findHideQueryCellOptions >>= leftClick >>> sequence
--
--showQueryCellOptions :: Check Unit
--showQueryCellOptions = findShowQueryCellOptions >>= leftClick >>> sequence
--
--hideMdCellOptions :: Check Unit
--hideMdCellOptions = findHideMdCellOptions >>= leftClick >>> sequence
--
--showMdCellOptions :: Check Unit
--showMdCellOptions = findShowMdCellOptions >>= leftClick >>> sequence

--hideExploreCellOptions :: Check Unit
--hideExploreCellOptions = findHideExploreCellOptions >>= leftClick >>> sequence
--
--showExploreCellOptions :: Check Unit
--showExploreCellOptions = findShowExploreCellOptions >>= leftClick >>> sequence
--
--hideSearchCellOptions :: Check Unit
--hideSearchCellOptions = findHideSearchCellOptions >>= leftClick >>> sequence
--
--showExploreCellOptions :: Check Unit
--showExploreCellOptions = findShowExploreCellOptions >>= leftClick >>> sequence

