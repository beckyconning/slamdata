module Test.Selenium.Notebook.Interactions where

import Control.Apply ((*>))
import Control.Bind ((<=<), (=<<))
import Control.Monad.Aff (later)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Random (randomInt)
import Data.Foldable (traverse_) as F
import Data.List (length, replicateM)
import Data.Traversable (traverse) as T
import Prelude
import Selenium.ActionSequence (leftClick)
import Selenium.Monad (tryRepeatedlyTo, refresh, byXPath, findElements)
import Selenium.Types (Element())
import Test.Selenium.Interactions (click, selectAll, typeString, pressEnter, selectAll, hover)
import Test.Selenium.Log (warnMsg)
import Test.Selenium.Monad (Check(), getConfig, getModifierKey)
import Test.Selenium.Finders (findByXPath)
import Test.Selenium.Notebook.Finders as Finders
import Test.Selenium.Common (waitTime)
import Test.XPath (anywhere, anyWithExactText)

findFile :: String -> Check Element
findFile name = findByXPath $ anywhere $ anyWithExactText name

browseFolder :: String -> Check Unit
browseFolder name = findByXPath (anywhere $ anyWithExactText name) >>= click

embedCellOutput :: Check Unit
embedCellOutput = click =<< Finders.findEmbedCellOutput

browseRootFolder :: Check Unit
browseRootFolder = Finders.findBrowseRootFolder >>= click

browseTestFolder :: Check Unit
browseTestFolder = browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: Check Unit
createNotebook = Finders.findCreateNotebook >>= click

nameNotebook :: String -> Check Unit
nameNotebook name = do
  Finders.findUntitledNotebookNameInput >>= click
  getModifierKey >>= selectAll
  typeString name
  pressEnter

deleteFile :: String -> Check Unit
deleteFile name = Finders.findRemoveFile name >>= click

createNotebookInTestFolder :: String -> Check Unit
createNotebookInTestFolder name = browseTestFolder *> createNotebook *> nameNotebook name

deleteFileInTestFolder :: String -> Check Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook :: Check Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCellMenu :: Check Unit
expandNewCellMenu = Finders.findInsertCell >>= click

insertCellUsingNextActionMenu :: Check Element -> Check Unit
insertCellUsingNextActionMenu findSpecificInsertCell =
  expandNewCellMenu *> findSpecificInsertCell >>= click

insertQueryCellUsingNextActionMenu :: Check Unit
insertQueryCellUsingNextActionMenu = insertCellUsingNextActionMenu Finders.findInsertQueryCell

insertMdCellUsingNextActionMenu :: Check Unit
insertMdCellUsingNextActionMenu = insertCellUsingNextActionMenu Finders.findInsertMdCell

insertExploreCellUsingNextActionMenu :: Check Unit
insertExploreCellUsingNextActionMenu = insertCellUsingNextActionMenu Finders.findInsertExploreCell

insertSearchCellUsingNextActionMenu :: Check Unit
insertSearchCellUsingNextActionMenu = insertCellUsingNextActionMenu Finders.findInsertSearchCell

insertRandomNumberOfCells :: Check Unit -> Check Int
insertRandomNumberOfCells insertCell = do
  numberOfCellsToInsert <- liftEff $ randomInt 1 10
  replicateM numberOfCellsToInsert insertCell
  pure numberOfCellsToInsert

insertRandomNumberOfQueryCells :: Check Int
insertRandomNumberOfQueryCells = insertRandomNumberOfCells insertQueryCellUsingNextActionMenu

insertRandomNumberOfMdCells :: Check Int
insertRandomNumberOfMdCells = insertRandomNumberOfCells insertMdCellUsingNextActionMenu

insertRandomNumberOfExploreCells :: Check Int
insertRandomNumberOfExploreCells = insertRandomNumberOfCells insertExploreCellUsingNextActionMenu

insertRandomNumberOfSearchCells :: Check Int
insertRandomNumberOfSearchCells = insertRandomNumberOfCells insertSearchCellUsingNextActionMenu

-- Finds at least 1 cell and deletes it.
deleteAllCells :: Check Unit
deleteAllCells = Finders.findAllDeleteCellOptions >>= F.traverse_ click

-- Deletes any cells that there are.
deleteAnyCells :: Check Unit
deleteAnyCells = Finders.findAnyDeleteCellOptions >>= F.traverse_ click

showQueryCellOptions :: Check Unit
showQueryCellOptions = Finders.findShowQueryCellOptions >>= click

hideQueryCellOptions :: Check Unit
hideQueryCellOptions = Finders.findHideQueryCellOptions >>= click

showMdCellOptions :: Check Unit
showMdCellOptions = Finders.findShowMdCellOptions >>= click

hideMdCellOptions :: Check Unit
hideMdCellOptions = Finders.findHideMdCellOptions >>= click

showFileList :: Check Unit
showFileList = Finders.findShowFileList >>= click

hideFileList :: Check Unit
hideFileList = Finders.findHideFileList >>= click

selectFileFromInitialFileList :: String -> Check Unit
selectFileFromInitialFileList = click <=< Finders.findFileFromFileList

showExploreCellOptions :: Check Unit
showExploreCellOptions = Finders.findShowExploreCellOptions >>= click

hideExploreCellOptions :: Check Unit
hideExploreCellOptions = Finders.findHideExploreCellOptions >>= click

showSearchCellOptions :: Check Unit
showSearchCellOptions = Finders.findShowSearchCellOptions >>= click

hideSearchCellOptions :: Check Unit
hideSearchCellOptions = Finders.findHideSearchCellOptions >>= click

