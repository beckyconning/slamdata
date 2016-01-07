module Test.Selenium.Notebook.Expectations where

import Prelude
import Test.Selenium.Monad (Check())
import Test.Selenium.Finders (findByXPath)
import Test.Selenium.Notebook.Data (initialFileList)
import Data.Foldable (traverse_)
import Test.XPath as XPath

--expectInputWithValue :: String -> Check Unit
--expectInputWithValue = void <<< findByXPath <<< XPath.inputWithExactText

