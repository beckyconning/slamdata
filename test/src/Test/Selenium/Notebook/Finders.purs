{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http:www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Selenium.Notebook.Finders where

import Data.List (List())
import Data.Traversable (traverse)
import Control.Bind ((<=<))
import Prelude
import Selenium.Monad (loseElement, findElements, findExact, byXPath, tryRepeatedlyTo)
import Test.Selenium.Finders (findInputWithPlaceholderAndValue, findByXPathAndProperty, findByXPath, findFirstByXPath, findAllByXPath, findAnyByXPath, loseByXPath)
import Selenium.Types (Element())
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath
import Test.Selenium.Monad (Check())

-- Finders
findInitialFileListInOrder :: Check Element
findInitialFileListInOrder = findByXPath $ XPath.anywhere XPaths.initialFileListInOrderXPath

loseInitialFileList :: Check Unit
loseInitialFileList = void $ traverse loseByXPath $ map XPath.anywhere XPaths.initialFileListXPaths

findFileFromFileList :: String -> Check Element
findFileFromFileList = findByXPath <<< XPath.anywhere <<< XPath.anyWithExactText

findEmbedCellOutput :: Check Element
findEmbedCellOutput = findByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

loseEmbedCellOutput :: Check Unit
loseEmbedCellOutput = loseByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

loseInsertQueryAfterThis :: Check Unit
loseInsertQueryAfterThis = loseByXPath $ XPath.anywhere XPaths.insertQueryAfterThisXPath

loseInsertSearchAfterThis :: Check Unit
loseInsertSearchAfterThis = loseByXPath $ XPath.anywhere XPaths.insertSearchAfterThisXPath

loseInsertVisualizeAfterThis :: Check Unit
loseInsertVisualizeAfterThis = loseByXPath $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

loseInsertDownloadAfterThis :: Check Unit
loseInsertDownloadAfterThis = loseByXPath $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

loseEmbedCellOutputSnippet :: Check Unit
loseEmbedCellOutputSnippet = loseByXPath $ XPath.anywhere XPaths.embedCellOutputSnippetXPath

loseEmbedCellOutputTitle :: Check Unit
loseEmbedCellOutputTitle = loseByXPath $ XPath.anywhere XPaths.embedCellOutputTitleXPath

findDismissInsertCellMenu :: Check Element
findDismissInsertCellMenu = findByXPath $ XPath.anywhere XPaths.dismissInsertCellMenuXPath

findInsertQueryCell :: Check Element
findInsertQueryCell = findByXPath $ XPath.anywhere XPaths.insertQueryCellXPath

findInsertMdCell :: Check Element
findInsertMdCell = findByXPath $ XPath.anywhere XPaths.insertMdCellXPath

findInsertExploreCell :: Check Element
findInsertExploreCell = findByXPath $ XPath.anywhere XPaths.insertExploreCellXPath

findInsertSearchCell :: Check Element
findInsertSearchCell = findByXPath $ XPath.anywhere XPaths.insertSearchCellXPath

findInsertCell :: Check Element
findInsertCell = findByXPath $ XPath.anywhere XPaths.insertCellXPath

findAllQueryCellTitles :: Check (List Element)
findAllQueryCellTitles = findAllByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

findAllExploreCellTitles :: Check (List Element)
findAllExploreCellTitles = findAllByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

findAllSearchCellTitles :: Check (List Element)
findAllSearchCellTitles = findAllByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

findAllMdCellTitles :: Check (List Element)
findAllMdCellTitles = findAllByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

loseCellTitles :: Check Unit
loseCellTitles = loseByXPath $ XPath.anywhere XPaths.cellTitleXPath

loseQueryCellTitles :: Check Unit
loseQueryCellTitles = loseByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

loseExploreCellTitles :: Check Unit
loseExploreCellTitles = loseByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

loseSearchCellTitles :: Check Unit
loseSearchCellTitles = loseByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

loseMdCellTitles :: Check Unit
loseMdCellTitles = loseByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

findShowFileList :: Check Element
findShowFileList = findByXPath $ XPath.anywhere XPaths.showFileListXPath

findHideFileList :: Check Element
findHideFileList = findByXPath $ XPath.anywhere XPaths.hideFileListXPath

findAllDeleteCellOptions :: Check (List Element)
findAllDeleteCellOptions = findAllByXPath $ XPath.anywhere XPaths.deleteCellXPath

findAnyDeleteCellOptions :: Check (List Element)
findAnyDeleteCellOptions = findAnyByXPath $ XPath.anywhere XPaths.deleteCellXPath

findIndexedQueryCellTitle :: Int -> Check Element
findIndexedQueryCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.queryCellTitleXPath)

findIndexedMdCellTitle :: Int -> Check Element
findIndexedMdCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.mdCellTitleXPath)

findIndexedExploreCellTitle :: Int -> Check Element
findIndexedExploreCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.exploreCellTitleXPath)

findIndexedSearchCellTitle :: Int -> Check Element
findIndexedSearchCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.searchCellTitleXPath)

findHideQueryCellOptions :: Check Element
findHideQueryCellOptions = findByXPath $ XPath.anywhere XPaths.hideQueryCellOptionsXPath

findHideMdCellOptions :: Check Element
findHideMdCellOptions = findByXPath $ XPath.anywhere XPaths.hideMdCellOptionsXPath

findHideExploreCellOptions :: Check Element
findHideExploreCellOptions = findByXPath $ XPath.anywhere XPaths.hideExploreCellOptionsXPath

findHideSearchCellOptions :: Check Element
findHideSearchCellOptions = findByXPath $ XPath.anywhere XPaths.hideSearchCellOptionsXPath

findShowQueryCellOptions :: Check Element
findShowQueryCellOptions = findByXPath $ XPath.anywhere XPaths.showQueryCellOptionsXPath

findShowMdCellOptions :: Check Element
findShowMdCellOptions = findByXPath $ XPath.anywhere XPaths.showMdCellOptionsXPath

findShowExploreCellOptions :: Check Element
findShowExploreCellOptions = findByXPath $ XPath.anywhere XPaths.showExploreCellOptionsXPath

findShowSearchCellOptions :: Check Element
findShowSearchCellOptions = findByXPath $ XPath.anywhere XPaths.showSearchCellOptionsXPath

findExploreInput :: Check Element
findExploreInput = findByXPath $ XPath.anywhere XPaths.exploreInputXPath

loseExploreInput :: Check Unit
loseExploreInput = loseByXPath $ XPath.anywhere XPaths.exploreInputXPath

findBrowseRootFolder :: Check Element
findBrowseRootFolder = findByXPath $ XPath.anywhere XPaths.browseRootFolderXPath

findRemoveFile :: String -> Check Element
findRemoveFile = findFirstByXPath <<< XPath.anywhere <<< XPaths.removeFileXPath

findCreateNotebook :: Check Element
findCreateNotebook = findFirstByXPath $ XPath.anywhere XPaths.createNotebookXPath

findUntitledNotebookNameInput :: Check Element
findUntitledNotebookNameInput =
  findByXPathAndProperty (XPath.anywhere "input") "value" "Untitled Notebook"

findSelectFileInputWithValue :: String -> Check Element
findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

