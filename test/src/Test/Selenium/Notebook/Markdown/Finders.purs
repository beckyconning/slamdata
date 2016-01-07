module Test.Selenium.Notebook.Markdown.Finders where

import Prelude

import Test.Selenium.Monad (Check(), byAriaLabel)
import Test.Selenium.Finders (findByXPath, findAllByXPath, findAnyByXPath)
import Data.List (List(), length)
import Selenium.Monad (byXPath, tryRepeatedlyTo, getText, findElements)
import Selenium.Combinators (tryToFind)
import Selenium.Types (Element())
import Test.XPath as XPath
import Test.Selenium.XPaths as XPaths

import Data.Traversable (traverse) as T

markdownQueryTitleXPath :: String
markdownQueryTitleXPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.queryCellTitleXPath

findMdField :: Check Element
findMdField = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath

findMdPlayButton :: Check Element
findMdPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath

findMdQueryPlayButton :: Check Element
findMdQueryPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = markdownQueryTitleXPath `XPath.following` XPaths.playXPath

findCreateMdQueryCellButton :: Check Element
findCreateMdQueryCellButton =
  findByXPath $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

findMdQueryField :: Check Element
findMdQueryField = findByXPath $ XPath.anywhere xPath
  where
  xPath = markdownQueryTitleXPath `XPath.following` XPaths.aceEditorXPath

findMdQueryColumnCellsByIndex :: Int -> Check (List Element)
findMdQueryColumnCellsByIndex index = findAllByXPath $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = markdownQueryTitleXPath `XPath.following` "tbody/tr/td"

findMdQueryColumnCellsByHeading :: String -> Check (List Element)
findMdQueryColumnCellsByHeading heading = findHeaderIndex >>= findMdQueryColumnCellsByIndex
  where
  columnHeadingXPath = XPath.nodeWithExactText "thead/tr/th"
  findHeaderIndex = ((+ 1) <<< length) <$> (findAnyByXPath $ XPath.anywhere priorHeadingXPath)
  priorHeadingXPath =
    markdownQueryTitleXPath `XPath.following` columnHeadingXPath heading `XPath.precedingSibling` XPath.any

findMdQueryColumnCellsTextByHeading :: String -> Check (List String)
findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= T.traverse getText

