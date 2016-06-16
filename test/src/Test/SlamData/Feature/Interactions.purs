module Test.SlamData.Feature.Interactions where

import SlamData.Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Arr
import Data.Map as Map
import Data.String as Str
import Selenium.Monad (get, refresh, getCurrentUrl, tryRepeatedlyTo)
import Test.Feature as Feature
import Test.Feature.Log as Log
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Expectations as Expect
import Test.Utils (appendToCwd)
import XPath as XPath

nthDeckXPath :: Int -> String
nthDeckXPath = XPath.index $ XPath.anywhere $ XPaths.deck

inNthDeck :: Int -> String -> String
inNthDeck index = XPath.descendant $ nthDeckXPath index

launchSlamData ∷ SlamFeature Unit
launchSlamData = get ∘ _.slamdataUrl =<< getConfig

accessWorkspaceWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessWorkspaceWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get

mountTestDatabase ∷ SlamFeature Unit
mountTestDatabase = do
  Feature.click (XPath.anywhere XPaths.accessMountDatabase)
  Feature.provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  Feature.selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  Feature.click (XPath.anywhere XPaths.mountButton)

accessFile ∷ String → SlamFeature Unit
accessFile =
  Feature.click ∘ XPath.anywhere ∘ XPaths.accessFile

exploreFile ∷ SlamFeature Unit
exploreFile =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Explore file"

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = Feature.click ∘ XPath.anywhere ∘ XPaths.accessBreadcrumb

embedCardOutput ∷ SlamFeature Unit
embedCardOutput = Feature.click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = do
  tryRepeatedlyTo do
    ((Feature.clickNotRepeatedly $ XPath.anywhere XPaths.headerGripper)
     <|>
     (Feature.clickNotRepeatedly $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1))
    Expect.fileNotRepeatedly "test-mount"

browseTestFolder ∷ SlamFeature Unit
browseTestFolder =
  browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createWorkspace ∷ SlamFeature Unit
createWorkspace = Feature.click $ XPath.anywhere XPaths.createWorkspace

nameWorkspace ∷ String → SlamFeature Unit
nameWorkspace name = do
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just "Untitled Workspace")
    (XPath.anywhere "input")
    name
  Feature.pressEnter

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
    *> (Feature.click (XPath.anywhere $ XPaths.removeFile name)
          *> Feature.expectNotPresented (XPath.anywhere (XPaths.removeFile name))
          <|> Log.warnMsg "Couldn't remove file, see https://slamdata.atlassian.net/browse/SD-1613 and https://slamdata.atlassian.net/browse/SD-1614")

shareFile ∷ String → SlamFeature Unit
shareFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name) *> Feature.click (XPath.anywhere $ XPaths.shareFile name)

renameFile ∷ String → String → SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  Feature.click $ XPath.anywhere $ XPaths.moveFile oldName
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just oldName)
    (XPath.anywhere "input")
    newName
  Feature.click $ XPath.anywhere XPaths.renameButton

moveFile ∷ String → String → String → SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.moveFile fileName
  Feature.click $ XPath.anywhere XPaths.selectADestinationFolder
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  Feature.click $ XPath.anywhere XPaths.renameButton

uploadFile ∷ String → SlamFeature Unit
uploadFile =
  Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
    <=< liftEff
    ∘ appendToCwd

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  Feature.provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

selectFile ∷ String → SlamFeature Unit
selectFile name =
  Feature.click $ XPath.anywhere $ XPaths.selectFile name

createWorkspaceInTestFolder ∷ String → SlamFeature Unit
createWorkspaceInTestFolder name = do
  browseTestFolder
  createWorkspace
  Feature.expectPresented
    $ XPath.anywhere
    $ XPaths.headerGripper

createFolder ∷ SlamFeature Unit
createFolder = Feature.click $ XPath.anywhere XPaths.createFolder

deleteFileInTestFolder ∷ String → SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentWorkspace ∷ SlamFeature Unit
reopenCurrentWorkspace = waitTime 2000 *> refresh

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = Feature.click (XPath.anywhere XPaths.insertCard)

accessNextCardInNthDeck ∷ Int → SlamFeature Unit
accessNextCardInNthDeck index =
  Feature.dragAndDrop
    Feature.Center
    (inNthDeck index XPaths.enabledNextCardGripper)
    (inNthDeck index XPaths.previousCardGripper)

accessPreviousCardInNthDeck ∷ Int → SlamFeature Unit
accessPreviousCardInNthDeck index =
  Feature.dragAndDrop
    Feature.Center
    (inNthDeck index XPaths.enabledPreviousCardGripper)
    (inNthDeck index XPaths.nextCardGripper)

insertSearchCardInNthDeck ∷ Int → SlamFeature Unit
insertSearchCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertSearchCard

insertQueryCardInNthDeck ∷ Int → SlamFeature Unit
insertQueryCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertQueryCard

insertMdCardInNthDeck ∷ Int → SlamFeature Unit
insertMdCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertMdCard

insertExploreCardInNthDeck ∷ Int → SlamFeature Unit
insertExploreCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertExploreCard

insertVisualizeCardInNthDeck ∷ Int → SlamFeature Unit
insertVisualizeCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertVisualizeCard

insertDisplayMarkdownCardInNthDeck ∷ Int → SlamFeature Unit
insertDisplayMarkdownCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertDisplayMarkdownCard

insertJTableCardInNthDeck ∷ Int → SlamFeature Unit
insertJTableCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertJTableCard

insertChartCardInNthDeck ∷ Int → SlamFeature Unit
insertChartCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertChartCard

insertCacheCardInNthDeck ∷ Int → SlamFeature Unit
insertCacheCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertCacheCard

insertApiCardInNthDeck ∷ Int → SlamFeature Unit
insertApiCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertApiCard

insertApiResultsCardInNthDeck ∷ Int → SlamFeature Unit
insertApiResultsCardInNthDeck index =
  Feature.click $ inNthDeck index XPaths.insertApiResultsCard

selectFileForLastExploreCard ∷ String → SlamFeature Unit
selectFileForLastExploreCard p = do
  Expect.resourceOpenedInLastExploreCard "/"
  for_ paths \path → do
    Feature.click $ resourceXPath path
    Expect.resourceOpenedInLastExploreCard path
  where
  resourceXPath ∷ String → String
  resourceXPath rPath =
    XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel $ "Select " ⊕ rPath

  -- Constructs ["/foo/", "/foo/bar/", "/foo/bar/baz"] from "/foo/bar/baz"
  paths ∷ Array String
  paths =
    let
      parts = foldl foldFn [] $ Str.split "/" p
      mbUnconsed = Arr.uncons parts
    in Arr.drop 1 $ Arr.reverse case mbUnconsed of
      Nothing →
        parts
      Just {head, tail} →
        Arr.cons (fromMaybe head (Str.stripSuffix "/" head)) tail

  foldFn ∷ Array String → String → Array String
  foldFn acc new =
    Arr.cons (maybe "/" (\x → x ⊕ new ⊕ "/") $ Arr.head acc) acc

provideSearchStringInLastSearchCard ∷ String → SlamFeature Unit
provideSearchStringInLastSearchCard =
  Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.mdCardTitle
    `XPath.following` XPaths.aceEditor

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard =
  Feature.provideFieldValue
    $ (XPath.last $ XPath.anywhere $ XPaths.queryCardTitle)
    `XPath.following` XPaths.aceEditor

provideSaveDestinationInLastCacheCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastCacheCard =
  Feature.provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)

doSaveInLastCacheCard ∷ SlamFeature Unit
doSaveInLastCacheCard =
  Feature.click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

provideFieldValueInNthDeck ∷ Int → String → String → SlamFeature Unit
provideFieldValueInNthDeck index labelText =
  Feature.provideFieldValue
    $ inNthDeck index
    $ "input" `XPath.withLabelWithExactText` labelText

checkFieldInNthDeck ∷ Int → String → SlamFeature Unit
checkFieldInNthDeck index labelText =
  Feature.check
    $ inNthDeck index
    $ "input" `XPath.withLabelWithExactText` labelText

uncheckFieldInNthDeck ∷ Int → String → SlamFeature Unit
uncheckFieldInNthDeck index labelText =
  Feature.uncheck
    $ inNthDeck index
    $ "input" `XPath.withLabelWithExactText` labelText

pushRadioButtonInNthDeck ∷ Int → String → SlamFeature Unit
pushRadioButtonInNthDeck index labelText =
  Feature.pushRadioButton
    $ inNthDeck index
    $ "input" `XPath.withLabelWithExactText` labelText

selectFromDropdownInNthDeck ∷ Int → String → String → SlamFeature Unit
selectFromDropdownInNthDeck index labelText =
  Feature.selectFromDropdown
    $ inNthDeck index
    $ "select" `XPath.withLabelWithExactText` labelText

accessSharingUrl ∷ SlamFeature Unit
accessSharingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.sharingUrl

downloadFileAsCSV ∷ String → SlamFeature Unit
downloadFileAsCSV fileName = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadButton
  Feature.click $ XPath.anywhere $ XPaths.cancelButton

downloadFileAsJSON ∷ String → SlamFeature Unit
downloadFileAsJSON fileName = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
  Feature.click $ XPath.anywhere $ XPath.anyWithText "JSON"
  Feature.click $ XPath.anywhere $ XPaths.downloadButton
  Feature.click $ XPath.anywhere $ XPaths.cancelButton

showHiddenFiles ∷ SlamFeature Unit
showHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.showHiddenFiles

hideHiddenFiles ∷ SlamFeature Unit
hideHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.hideHiddenFiles

type ApiVarName = String
type ApiVarType = String
type ApiVarValue = String

provideApiVariableBindingsForApiCard
  ∷ ApiVarName
  → ApiVarType
  → ApiVarValue
  → SlamFeature Unit
provideApiVariableBindingsForApiCard name ty val =
  provideValueForApiCard name
  *> provideTypeForApiCard name ty
  *> provideDefaultValueForApiCard name val
  where
  provideValueForApiCard ∷ String → SlamFeature Unit
  provideValueForApiCard name = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableName)
      name
    Feature.pressEnter
  provideTypeForApiCard ∷ String → String → SlamFeature Unit
  provideTypeForApiCard name ty = do
    Feature.selectFromDropdown
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableTypeFor name)
      ty
    Feature.pressEnter

  provideDefaultValueForApiCard ∷ String → String → SlamFeature Unit
  provideDefaultValueForApiCard name val = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.apiCardDefaultValueFor name)
      val
    Feature.pressEnter

provideCategoryForLastVisualizeCard
  ∷ String
  → SlamFeature Unit
provideCategoryForLastVisualizeCard str =
  Feature.selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartCategory)
    str

provideDimensionForLastVisualizeCard
  ∷ String
  → SlamFeature Unit
provideDimensionForLastVisualizeCard str =
  Feature.selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartDimension)
    str

provideSeriesForLastVizualizeCard ∷ String → SlamFeature Unit
provideSeriesForLastVizualizeCard str =
  Feature.selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartSeries)
    str

switchToBarChart ∷ SlamFeature Unit
switchToBarChart =
  Feature.click $ XPath.anywhere $ XPaths.chartSwitchToBar

switchToLineChart ∷ SlamFeature Unit
switchToLineChart =
  Feature.click $ XPath.anywhere $ XPaths.chartSwitchToLine

flipNthDeck ∷ Int → SlamFeature Unit
flipNthDeck index =
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Trash card"

shareDeck ∷ SlamFeature Unit
shareDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"

publishDeck ∷ SlamFeature Unit
publishDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

filterActions ∷ String → SlamFeature Unit
filterActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter actions")

wrapNthDeck ∷ Int → SlamFeature Unit
wrapNthDeck index =
  flipNthDeck index *> pressLastWrapButton
  where
  pressLastWrapButton = Feature.click $ XPath.last $ XPath.anywhere $ XPaths.wrap

moveNthDeckToLeftOfMthDeck ∷ Int → Int → SlamFeature Unit
moveNthDeckToLeftOfMthDeck dragIndex dropIndex =
  Feature.dragAndDrop
    Feature.Left
    (XPath.index (inNthDeck dragIndex XPaths.deckGripper) 1)
    (XPath.index (inNthDeck dropIndex XPaths.previousCardGripper) 1)

resizeNthDeckAllTheWayToTheRightOfMthDeck ∷ Int → Int → SlamFeature Unit
resizeNthDeckAllTheWayToTheRightOfMthDeck dragIndex dropIndex =
  Feature.dragAndDrop
    Feature.Left
    (XPath.index (inNthDeck dragIndex XPaths.resizeDeck) 1)
    (XPath.index (inNthDeck dropIndex XPaths.nextCardGripper) 1)

insertNewDeckInLastDraftboard ∷ SlamFeature Unit
insertNewDeckInLastDraftboard =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.draftboard
