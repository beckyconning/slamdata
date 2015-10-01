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

module Test.Selenium.Notebook.Markdown (test) where

import Prelude
import Debug.Trace (traceShow)

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import Data.Array ((..), (:))
import Data.Int (toNumber)
import Data.List (List(..), fromList, zipWith, take, length, toList, elemIndex)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.Regex (regex, noFlags)

import Control.Monad.List.Trans (ListT())

import Selenium.Types (Element(), Locator())
import Selenium.Monad (Selenium(), sequence, getAttribute, getText, findChildren, findExact, childExact, byXPath, byId, byCss, tryRepeatedlyTo, findElements, navigateTo)
import Selenium.ActionSequence (Sequence(), sendKeys, mouseDown, mouseUp, leftClick)
import Selenium.Key (shiftKey)
import Selenium.MouseButton (leftButton)
import Selenium.Combinators (tryToFind)
import Selenium (showLocator)

import Test.Config
import Test.Selenium.ActionSequence (selectAll, keys)
import Test.Selenium.Expect (Expectation(), expect, toEq, toNotEq, toBeGreaterThan, toMatch)
import Test.Selenium.Monad (Check(), getConfig, getModifierKey, byAriaLabel, byText, byExactText, findAtLeast, findSingle)
import Test.Selenium.Log (sectionMsg, successMsg, warnMsg)
import Test.Selenium.Notebook.Contexts (deleteAllCells, insertMdCell)

import Utils (s2i)

import qualified Data.Traversable (traverse, sequence) as T
import qualified Data.Foldable (sequence_, traverse_) as F

provideMd :: String -> Check Unit
provideMd md = focusMdField *> sequence (keys $ md ++ " ")

expectMdQueryColumn :: forall a. (Show a) => String -> Expectation String a -> a -> Check Unit
expectMdQueryColumn heading expectation expected = do
  cellTexts <- findMdQueryColumnCellsTextByHeading heading
  F.traverse_ (\cellText -> expect cellText expectation expected) cellTexts

focus :: Element -> Check Unit
focus element = do
  sequence do
    mouseDown leftButton element
    mouseUp leftButton element

findMdField :: Check Element
findMdField = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[contains(@class, 'ace_editor')]"

findMdPlayButton :: Check Element
findMdPlayButton = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[@aria-label='Play']"

findMdQueryPlayButton :: Check Element
findMdQueryPlayButton = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::*[@aria-label='Play']"

focusMdField :: Check Unit
focusMdField = findMdField >>= focus

changeMd :: String -> Check Unit
changeMd md = do
  focusMdField
  modifierKey <- getModifierKey
  sequence $ selectAll modifierKey *> keys md

playMd :: Check Unit
playMd = findMdPlayButton >>= sequence <<< leftClick

playMdQuery :: Check Unit
playMdQuery = findMdQueryPlayButton >>= sequence <<< leftClick

expectFinishedMessage :: Check Unit
expectFinishedMessage = do
  element <- tryToFind $ byText "Finished"
  message <- getText element
  expect message toMatch (regex "Finished: took ([0-9]*)ms." noFlags)

findElementIdByLabelText :: String -> Check String
findElementIdByLabelText text =
  tryRepeatedlyTo $ byXPath xPath >>= findSingle >>= findElementIdByLabel
    where
    xPath = "//label[text()='" ++ text ++ "']"
    findElementIdByLabel labelElement = getAttribute labelElement "for"

findElementByLabelText :: String -> Check Element
findElementByLabelText text = findElementIdByLabelText text >>= byId >>= findSingle

provideFieldValue :: String -> String -> Check Unit
provideFieldValue labelText value = do
  field <- findElementByLabelText labelText
  sequence $ leftClick field *> keys value

expectDropdownWithLabelOptionsAndValue :: String -> Array String -> String -> Check Unit
expectDropdownWithLabelOptionsAndValue expectedLabel expectedOptions expectedValue = do
  selectId <- findElementIdByLabelText expectedLabel
  select <- tryToFind $ byId selectId
  value <- getAttribute select "value"
  expect value toEq expectedValue
  void $ T.traverse (findOption select) expectedOptions
    where
    optionXPath text = "//option[text()=\"" ++ text ++ "\"]"
    findOption select text = byXPath (optionXPath text) >>= childExact select

expectInputWithLabelTypeAndValue :: String -> String -> String -> Check Unit
expectInputWithLabelTypeAndValue expectedLabel expectedInputType expectedValue = do
  inputId <- findElementIdByLabelText expectedLabel
  input <- tryToFind $ byId inputId
  value <- getAttribute input "value"
  inputType <- getAttribute input "type"
  expect (value :: String) toEq (expectedValue :: String)
  expect (inputType :: String) toEq (expectedInputType :: String)

expectLabel :: String -> Check Unit
expectLabel expected = void $ tryRepeatedlyTo $ byXPath labelXPath >>= findSingle
  where
  labelXPath = "//label[text()=\"" ++ expected ++ "\"]"

expectInputWithLabelTypeAndChecked :: String -> String -> Boolean -> Check Unit
expectInputWithLabelTypeAndChecked expectedLabel expectedType expectedChecked = do
  id <- findElementIdByLabelText expectedLabel
  void $ tryToFind $ byCss (inputSelector id expectedChecked)
    where
    baseSelector id = "input#" ++ id ++ "[type=\"" ++ expectedType ++ "\"]"
    inputSelector id true = baseSelector id ++ ":checked"
    inputSelector id false = baseSelector id ++ ":not(:checked)"

provideMdForFormWithAllInputTypes =
  T.traverse provideMd [ "discipline = __"
                       , "sport = __ (Bobsleigh)"
                       , "age = #__"
                       , "year = #__ (2002)"
                       , "startDate = __ - __ - __"
                       , "finishDate = __ - __ - __ (2002-06-06)"
                       , "startTime = __ : __"
                       , "finishTime = __ : __ (20:39)"
                       , "event = {1000m, 1500m, 3000m} (1500m)"
                       , "gender = []M []W []X"
                       , "color = [x]Red []Green [x]Blue"
                       , "type = (x)Gold ()Silver ()Bronze"
                       ]

expectToBePresentedWithFormWithAllInputTypes = do
  expectInputWithLabelTypeAndValue "discipline" "text" ""
  expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"

  expectInputWithLabelTypeAndValue "age" "number" ""
  expectInputWithLabelTypeAndValue "year" "number" "2002"

  expectInputWithLabelTypeAndValue "startDate" "text" ""
  expectInputWithLabelTypeAndValue "finishDate" "text" "2002-06-06"

  expectInputWithLabelTypeAndValue "startTime" "text" ""
  expectInputWithLabelTypeAndValue "finishTime" "text" "20:39"

  expectDropdownWithLabelOptionsAndValue "event" ["1000m", "1500m", "3000m"] "1500m"

  expectLabel "gender"
  expectInputWithLabelTypeAndChecked "X" "checkbox" false
  expectInputWithLabelTypeAndChecked "W" "checkbox" false
  expectInputWithLabelTypeAndChecked "M" "checkbox" false

  expectLabel "color"
  expectInputWithLabelTypeAndChecked "Red" "checkbox" true
  expectInputWithLabelTypeAndChecked "Green" "checkbox" false
  expectInputWithLabelTypeAndChecked "Blue" "checkbox" true

  expectLabel "type"
  expectInputWithLabelTypeAndChecked "Gold" "radio" true
  expectInputWithLabelTypeAndChecked "Silver" "radio" false
  expectInputWithLabelTypeAndChecked "Bronze" "radio" false

provideMdForFormWithEvaluatedContent =
    T.traverse provideMd [ "discipline = __ (!`SELECT discipline FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "year = #__ (!`SELECT year FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "country = {!`SELECT DISTINCT country FROM \"/test-mount/testDb/olympics\"`} (!`SELECT country FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "type = (!`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" LIMIT 1`) !`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" OFFSET 1`"
                         , "gender = [!`SELECT gender FROM \"/test-mount/testDb/olympics\" LIMIT 1`] !`SELECT DISTINCT gender FROM \"/test-mount/testDb/olympics\"`"
                         ]

expectToBePresentedWithFormWithEvaluatedContent = do
  expectInputWithLabelTypeAndValue "discipline" "text" "Figure skating"

  expectInputWithLabelTypeAndValue "year" "number" "1924"

  expectDropdownWithLabelOptionsAndValue "country" [ "LAT"
                                                   , "CZE"
                                                   , "UKR"
                                                   , "SLO"
                                                   , "RUS"
                                                   , "SVK"
                                                   , "KAZ"
                                                   , "AUS"
                                                   , "LUX"
                                                   , "UZB"
                                                   , "EUN"
                                                   , "DEN"
                                                   , "CHN"
                                                   , "ROU"
                                                   , "GDR"
                                                   , "PRK"
                                                   , "CRO"
                                                   , "URS"
                                                   , "BLR"
                                                   , "BUL"
                                                   , "POL"
                                                   , "EUA"
                                                   , "KOR"
                                                   , "NED"
                                                   , "ITA"
                                                   , "FRG"
                                                   , "EST"
                                                   , "SWE"
                                                   , "GBR"
                                                   , "TCH"
                                                   , "BEL"
                                                   , "FIN"
                                                   , "USA"
                                                   , "YUG"
                                                   , "SUI"
                                                   , "LIE"
                                                   , "CAN"
                                                   , "JPN"
                                                   , "HUN"
                                                   , "GER"
                                                   , "NOR"
                                                   , "NZL"
                                                   , "FRA"
                                                   , "AUT"
                                                   , "ESP"
                                                   ] "AUT"

  expectLabel "gender"
  expectInputWithLabelTypeAndChecked "X" "checkbox" false
  expectInputWithLabelTypeAndChecked "W" "checkbox" true
  expectInputWithLabelTypeAndChecked "M" "checkbox" false

  expectLabel "type"
  expectInputWithLabelTypeAndChecked "Gold" "radio" false
  expectInputWithLabelTypeAndChecked "Silver" "radio" true
  expectInputWithLabelTypeAndChecked "Bronze" "radio" false

findCreateMdQueryCellButton :: Check Element
findCreateMdQueryCellButton = tryToFind $ byAriaLabel "Query using fields"

createMdQueryCell :: Check Unit
createMdQueryCell = findCreateMdQueryCellButton >>= leftClick >>> sequence

findMdQueryField :: Check Element
findMdQueryField = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::*[contains(@class, 'ace_editor')]"

focusMdQueryField :: Check Unit
focusMdQueryField = findMdQueryField >>= focus

provideMdQuery :: String -> Check Unit
provideMdQuery query = focusMdQueryField *> (sequence $ keys query)

findElementIndexByText :: String -> Locator -> Check Int
findElementIndexByText text locator = do
  elements <- tryRepeatedlyTo $ findAtLeast 1 locator
  texts <- T.traverse getText elements
  case elemIndex text texts of
    Just i -> return i
    Nothing -> throwError $ error $ e
      where
      e = "Expected an element with xPath " ++ showLocator locator
                                            ++ " to have text "
                                            ++ show text ++ "."

findMdQueryColumnCellsByIndex :: Int -> Check (List Element)
findMdQueryColumnCellsByIndex index = tryRepeatedlyTo $ byXPath xPath >>= findElements
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::tbody/tr/td[" ++ show index ++ "]"

findMdQueryColumnCellsByHeading :: String -> Check (List Element)
findMdQueryColumnCellsByHeading heading =
  byXPath xPath >>= findElementIndexByText heading >>= findMdQueryColumnCellsByIndex
    where
    xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::thead/tr/td"

findMdQueryColumnCellsTextByHeading :: String -> Check (List String)
findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= T.traverse getText

test :: Check Unit
test = do
  --sectionMsg "Markdown: Provide and play markdown" *> do
  --  deleteAllCells

  --  insertMdCell
  --  provideMdForFormWithAllInputTypes
  --  playMd

  --  expectToBePresentedWithFormWithAllInputTypes
  --  expectFinishedMessage
  --  successMsg "Ok, succesfully provided and played markdown."

  --  deleteAllCells

  --sectionMsg "Markdown: Change and play markdown" *> do
  --  deleteAllCells

  --  insertMdCell
  --  provideMd "discipline = __"
  --  playMd
  --  changeMd "sport = __ (Bobsleigh)"
  --  playMd

  --  expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
  --  successMsg "Ok, successfully changed and played markdown."

  --  deleteAllCells

  --sectionMsg "Markdown: Provide and play markdown with evaluated content" *> do
  --  deleteAllCells

  --  insertMdCell
  --  provideMdForFormWithEvaluatedContent
  --  playMd

  --  expectToBePresentedWithFormWithEvaluatedContent
  --  successMsg "Ok, successfully provided and played markdown with evaluated content"

  --  deleteAllCells

  sectionMsg "Markdown: Filter query resuts by changing field values" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQuery """SELECT * FROM "/test-mount/testDb/olympics" WHERE discipline = :discipline
                                                                  AND   year       > :year"""
    playMdQuery

    provideFieldValue "discipline" "Bobsleigh"
    provideFieldValue "year" "1950"
    provideFieldValue "country" "GDR"
    provideFieldValue "gender" "M"
    provideFieldValue "type" "Gold"

    expectMdQueryColumn "discipline" toEq "Bobsleigh"
    expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1950
    expectMdQueryColumn "country" toEq "GDR"
    expectMdQueryColumn "gender" toEq "M"
    expectMdQueryColumn "type" toNotEq "Gold"

    successMsg "Ok, Filtered query results by changing field values"

  sectionMsg "Markdown: Filter query resuts with default field values" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQuery """SELECT * FROM "/test-mount/testDb/olympics" WHERE discipline =  :discipline
                                                                  AND   type       !=  :type
                                                                  AND   gender     =  :gender
                                                                  AND   year       >  :year
                                                                  AND   city       = :city"""
    playMdQuery

    expectMdQueryColumn "discipline" toEq "Figure skating"
    expectMdQueryColumn "country" toEq "AUT"
    expectMdQueryColumn "gender" toEq "W"
    expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1924
    expectMdQueryColumn "type" toNotEq "Gold"

    successMsg "Ok, Filtered query resuts with fields"
