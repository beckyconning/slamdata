module Test.XPath where

import Prelude
import Data.String (take, joinWith)
import Data.Foldable (Foldable, intercalate)
import Control.Alt (Alt)
import Control.Monad.Aff (later)
import Test.Utils (orIfItFails)
import Control.Bind ((<=<))
import Data.Maybe (Maybe(), maybe)

followingString :: String
followingString = "/following::"

following :: String -> String -> String
following x y = x ++ followingString ++ y

inOrder :: forall m. (Foldable m) => m String -> String
inOrder = intercalate followingString

index :: String -> Int -> String
index xPath = indexString xPath <<< show

indexString :: String -> String -> String
indexString xPath i = "(" ++ xPath ++ ")[" ++ i ++ "]"

withTextGreaterThan :: String -> String
withTextGreaterThan s = "text() > " ++ show s ++ ""

withTextLessThan :: String -> String
withTextLessThan s = "text() > " ++ show s ++ ""

withText :: String -> String
withText s = "text() = " ++ show s ++ ""

withoutText :: String -> String
withoutText s = "text() != " ++ show s ++ ""

predicate :: String -> String
predicate s = "[" ++ s ++ "]"

nodeWithExactText :: String -> String -> String
nodeWithExactText name text = name ++ (predicate $ withText text)

nodeWithText :: String -> String -> String
nodeWithText name text = name ++ "[contains(text(), '" ++ text ++ "')]"

nodeWithExactAttribute :: String -> String -> String -> String
nodeWithExactAttribute attribute name text = name ++ "[@" ++ attribute ++ "='" ++ text ++ "']"

nodeWithAttribute :: String -> String -> String -> String
nodeWithAttribute attribute name text = name ++ "[contains(@" ++ attribute ++ ", '" ++ text ++ "')]"

nodeWithExactAriaLabel :: String -> String -> String
nodeWithExactAriaLabel = nodeWithExactAttribute "aria-label"

nodeWithAriaLabel :: String -> String -> String
nodeWithAriaLabel = nodeWithAttribute "aria-label"

anyWithExactText :: String -> String
anyWithExactText = nodeWithExactText any

anyWithText :: String -> String
anyWithText = nodeWithText any

anyWithExactAriaLabel :: String -> String
anyWithExactAriaLabel = nodeWithExactAriaLabel any

anyWithAriaLabel :: String -> String
anyWithAriaLabel = nodeWithAriaLabel any

inputWithExactPlaceholder :: String -> String
inputWithExactPlaceholder = nodeWithExactAttribute "placeholder" "input"

inputWithPlaceholder :: String -> String
inputWithPlaceholder = nodeWithAttribute "placeholder" "input"

withLabel :: String -> String -> String
withLabel xPath labelXPath = xPath ++ "[@id=(" ++ labelXPath ++ "/@for)]"

thWithText :: String -> String -> String
thWithText tableXPath thText = tableXPath ++ "/thead/tr/th[text()='" ++ thText ++ "']"

tdWithTh :: String -> String -> String -> String
tdWithTh tableXPath thXPath tdXPath = indexString unindexedTdXPath thIndex
  where
  precedingThXPath = thXPath `precedingSibling` "th"
  thIndex = "(count(" ++ precedingThXPath ++ ") + 1)"
  unindexedTdXPath = tableXPath ++ "/tbody/tr/" ++ tdXPath

parent :: String -> String
parent xPath = xPath ++ "/.."

thisOrItsParents :: forall a m. (Alt m) => (String -> m a) -> String -> m a
thisOrItsParents f =
  orIfItFails f (orIfItFails (thisOrItsParents f <<< parent) f)

anywhere :: String -> String
anywhere xPath = if anywhered then xPath else "//" ++ xPath
  where
  anywhered = take 2 xPath == "//"

precedingSibling :: String -> String -> String
precedingSibling x y = x ++ "/preceding-sibling::" ++ y

any :: String
any = "*"

errorMessage :: String -> String -> String
errorMessage errorPartial xPath = errorPartial ++ " using the xPath: " ++ xPath ++ "."

tdWithThAndPredicate :: String -> String -> String -> String
tdWithThAndPredicate tableXPath thXPath predicate' =
  tdWithTh tableXPath thXPath tdXPath
  where
  tdXPath = "td" ++ predicate'

tdWithThAndTextEq :: String -> String -> String -> String
tdWithThAndTextEq tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< withText

tdWithThAndTextNotEq :: String -> String -> String -> String
tdWithThAndTextNotEq tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< withoutText

tdWithThAndTextGT :: String -> String -> String -> String
tdWithThAndTextGT tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< withTextGreaterThan

tdWithThAndTextLT :: String -> String -> String -> String
tdWithThAndTextLT tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< withTextLessThan

tdWithThAndTextEqOneOf :: String -> String -> Array String -> String
tdWithThAndTextEqOneOf tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< anyOfThesePredicates <<< map withText

tdWithThAndTextNotEqOneOf :: String -> String -> Array String -> String
tdWithThAndTextNotEqOneOf tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath <<< anyOfThesePredicates <<< map withoutText

anyOfThesePredicates :: Array String -> String
anyOfThesePredicates =
  predicate <<< joinWith " or "
