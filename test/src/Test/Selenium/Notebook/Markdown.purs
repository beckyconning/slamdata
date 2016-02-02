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
import Selenium.Monad (attempt)
import Control.Apply ((*>))
import Test.Selenium.Expectations (expectInputWithLabelTypeAndValue)
import Test.Selenium.Log (successMsg)
import Test.Selenium.Common (waitTime)
import Test.Selenium.Monad (Check())
import Test.Selenium.Notebook.Finders (loseCellTitles)
import Test.Selenium.Notebook.Interactions (createNotebookInTestFolder, deleteFileInTestFolder, insertMdCellUsingNextActionMenu)
import Test.Selenium.Notebook.Markdown.Expectations
import Test.Selenium.Notebook.Markdown.Interactions
import Test.Selenium.Scenario (scenario)

mdScenario :: String -> Array String -> Check Unit -> Check Unit
mdScenario =
  scenario
    "Markdown"
    (createNotebookInTestFolder "Markdown")
    (deleteFileInTestFolder "Markdown.slam")


evalDefaultValueIssues :: Array String
evalDefaultValueIssues = ["https://slamdata.atlassian.net/browse/SD-1048"]

test :: Check Unit
test = do
  mdScenario "Provide and play markdown" [] do
    insertMdCellUsingNextActionMenu
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectMdFinishedMessage
    successMsg "Ok, succesfully provided and played markdown."

  mdScenario "Change and play markdown" [] do
    insertMdCellUsingNextActionMenu
    provideMd "discipline = __"
    playMd
    changeMd "sport = __ (Bobsleigh)"
    playMd

    expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

  mdScenario "Provide and play markdown with evaluated content" [] do
    insertMdCellUsingNextActionMenu
    provideMdForFormWithEvaluatedContent
    playMd

    expectToBePresentedWithFormWithEvaluatedContent
    successMsg "Ok, successfully provided and played markdown with evaluated content"

  mdScenario "Filter query results with default field values" evalDefaultValueIssues do
    insertMdCellUsingNextActionMenu
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    expectMdQueryResultsToBeFilteredByDefaultFormValues

    successMsg "Ok, Filtered query resuts with fields"

  mdScenario "Filter query resuts by changing field values" [] do
    insertMdCellUsingNextActionMenu
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    changeAllFieldsInMdFormWithEvaluatedContent

    expectMdQueryResultsToBeFilteredByChangedFormValues

    successMsg "Ok, Filtered query results by changing field values"

