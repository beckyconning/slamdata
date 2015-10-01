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
import Test.Selenium.Log (sectionMsg, successMsg, warnMsg, errorMsg)
import Test.Selenium.Notebook.Contexts (deleteAllCells)
import Test.Selenium.Monad (Check())
import Test.Selenium.Expectations (expectInputWithLabelTypeAndValue)

import Test.Selenium.Notebook.Markdown.Interactions
import Test.Selenium.Notebook.Markdown.Expectations

import Utils (s2i)

import qualified Data.Traversable (traverse, sequence) as T
import qualified Data.Foldable (sequence_, traverse_) as F

test :: Check Unit
test = do
  sectionMsg "Markdown: Provide and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectMdFinishedMessage
    successMsg "Ok, succesfully provided and played markdown."

    deleteAllCells

  sectionMsg "Markdown: Change and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMd "discipline = __"
    playMd
    changeMd "sport = __ (Bobsleigh)"
    playMd

    expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

    deleteAllCells

  sectionMsg "Markdown: Provide and play markdown with evaluated content" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    expectToBePresentedWithFormWithEvaluatedContent
    successMsg "Ok, successfully provided and played markdown with evaluated content"

    deleteAllCells

  sectionMsg "Markdown: Filter query resuts with default field values" *> (do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    expectMdQueryResultsToBeFilteredByDefaultFormValues

    successMsg "Ok, Filtered query resuts with fields"
    errorMsg "Ok despite known issues, if resolved please remove known issues alternative.")
      <|> warnMsg "Warning, these known issues caused this scenario to fail:\n\
                  \  https://slamdata.atlassian.net/browse/SD-1046\n\
                  \  https://slamdata.atlassian.net/browse/SD-1045\n\
                  \  https://slamdata.atlassian.net/browse/SD-1044\n\
                  \  https://slamdata.atlassian.net/browse/SD-1047\n\
                  \  https://slamdata.atlassian.net/browse/SD-1048"

  sectionMsg "Markdown: Filter query resuts by changing field values" *> (do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    changeAllFieldsInMdFormWithEvaluatedContent

    expectMdQueryResultsToBeFilteredByChangedFormValues

    successMsg "Ok, Filtered query results by changing field values"
    errorMsg "Ok despite known issues, if resolved please remove known issues alternative.")
      <|> warnMsg "Warning, these known issues caused this scenario to fail:\n\
                  \  https://slamdata.atlassian.net/browse/SD-1046\n\
                  \  https://slamdata.atlassian.net/browse/SD-1045\n\
                  \  https://slamdata.atlassian.net/browse/SD-1044\n\
                  \  https://slamdata.atlassian.net/browse/SD-1047"
