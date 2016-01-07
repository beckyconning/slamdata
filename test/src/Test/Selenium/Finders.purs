module Test.Selenium.Finders where

import Prelude

import Control.Apply ((*>))
import Control.Bind ((<=<), (=<<))
import Control.Alt (Alt, (<|>))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.List (List(), uncons, length, elemIndex, zip, filter, head)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.String (take)
import Selenium (showLocator)
import Selenium.Monad (getAttribute, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml)
import Selenium.ActionSequence (hover)
import Selenium.Types (Element(), Locator())
import Test.Selenium.Common (attrFail)
import Test.Selenium.Monad (Check())
import Test.XPath as XPath

import Data.Traversable (traverse)
import Data.Foldable (traverse_)

findParent :: Element -> Check Element
findParent element = childExact element =<< byXPath ".."

findSingle :: Locator -> Check Element
findSingle locator = do
  elements <- findElements locator
  case uncons elements of
    Nothing ->
      throwLocatorError "Couldn't find an element with the locator: " locator
    Just o -> case length o.tail of
      0 -> pure $ o.head
      _ -> throwLocatorError "Found more than one element with the locator: " locator

findAtLeast :: Int -> Locator -> Check (List Element)
findAtLeast n locator = do
  elements <- findElements locator
  if length elements >= n
    then pure $ elements
    else
      throwLocatorError
        ("Couldn't find at least " ++ show n ++ " elements with the locator")
        locator

throwLocatorError :: forall a. String -> Locator -> Check a
throwLocatorError errorPartial = throwString <<< concatinateWithErrorPartial <<< showLocator
  where
  throwString = throwError <<< error
  concatinateWithErrorPartial locator = errorPartial ++ ": " ++ locator ++ "."

throwMissingAttributeError :: forall a. String -> String -> Check a
throwMissingAttributeError attr = throwLocatorError message <=< byXPath
  where
  message = "Expected non null " ++ show attr ++ " attribute for the element located by"

throwNoElementWithPropertyError :: forall a. String -> String -> String -> Check a
throwNoElementWithPropertyError name value = throwLocatorError message <=< byXPath
  where
  message =
    "Unable to locate element with "
      ++ show name
      ++ " property of "
      ++ show value
      ++ " using the locator"

expectPresented :: Locator -> Element -> Check Unit
expectPresented locator element = expectPresentedVisual *> expectPresentedAria
  where
  expectPresentedVisual = isDisplayed element >>= ifFalse (throwLocatorError visualError locator)
  expectPresentedAria = getAttribute element "aria-hidden" >>= validateAriaHiddenAttribute
  validateAriaHiddenAttribute (Just "true") = throwLocatorError ariaError locator
  validateAriaHiddenAttribute _ = pure unit
  visualError = "Expected to be able to mouse over the element located by"
  ariaError = "Expected attribute aria-hidden not to be \"true\" for the element located by"

expectHidden :: Locator -> Element -> Check Unit
expectHidden locator element = expectHiddenVisual *> expectHiddenAria
  where
  expectHiddenVisual = isDisplayed element >>= ifTrue (throwLocatorError visualError locator)
  expectHiddenAria = thisOrItsParents expectHiddenAria' element
  expectHiddenAria' element' = getAttribute element' "aria-hidden" >>= validateAriaHiddenAttribute
  validateAriaHiddenAttribute (Just "false") = throwLocatorError ariaError locator
  validateAriaHiddenAttribute Nothing = throwLocatorError ariaError locator
  validateAriaHiddenAttribute _ = pure unit
  ariaError = "Expected attribute aria-hidden to be \"true\" for the element (or one of its parents) located by"
  visualError = "Expected not to be able to mouse over the element located by"

thisOrItsParents :: forall a. (Element -> Check a) -> Element -> Check a
thisOrItsParents f = later 0 <<< orIfItFails f (orIfItFails (thisOrItsParents f <=< findParent) f)

ifFalse :: forall m. (Applicative m) => m Unit -> Boolean -> m Unit
ifFalse f boolean = if boolean then pure unit else f

ifTrue :: forall m. (Applicative m) => m Unit -> Boolean -> m Unit
ifTrue f boolean = if boolean then f else pure unit

passover :: forall a b m. (Applicative m) => (a -> m b) -> a -> m a
passover f x = f x *> pure x

orIfItFails :: forall a b m. (Alt m) => (a -> m b) -> (a -> m b) -> a -> m b
orIfItFails f g x = f x <|> g x

findElementIdByLabelText :: String -> Check String
findElementIdByLabelText text = do
  findLabel >>= getFor >>= maybe (throwMissingAttributeError attr xPath) pure
  where
  findLabel = findByXPath xPath
  attr = "for"
  getFor = flip getAttribute attr
  xPath = XPath.anywhere $ XPath.nodeWithExactText "label" text

findByLabelText :: String -> Check Element
findByLabelText = findSingle <=< byId <=< findElementIdByLabelText

findByXPath' :: String -> Check Element
findByXPath' = findSingle <=< byXPath

findFirstByXPath' :: String -> Check Element
findFirstByXPath' = findExact <=< byXPath

findAnyByXPath' :: String -> Check (List Element)
findAnyByXPath' = findElements <=< byXPath

findAllByXPath' :: String -> Check (List Element)
findAllByXPath' = findAtLeast 1 <=< byXPath

loseByXPath :: String -> Check Unit
loseByXPath = tryRepeatedlyTo <<< (loseDOM `orIfItFails` losePresented) <=< byXPath
  where
  loseDOM = loseElement
  losePresented locator = traverse_ (expectHidden locator) =<< findElements locator

findByXPath :: String -> Check Element
findByXPath xPath = do
  locator <- byXPath xPath
  element <- tryRepeatedlyTo $ findByXPath' xPath
  expectPresented locator element
  pure element

findFirstByXPath :: String -> Check Element
findFirstByXPath xPath = do
  locator <- byXPath xPath
  element <- tryRepeatedlyTo $ findFirstByXPath' xPath
  expectPresented locator element
  pure element

findAllByXPath :: String -> Check (List Element)
findAllByXPath xPath = do
  locator <- byXPath xPath
  elements <- tryRepeatedlyTo $ findAllByXPath' xPath
  traverse_ (expectPresented locator) elements
  pure elements

findAnyByXPath :: String -> Check (List Element)
findAnyByXPath xPath = do
  locator <- byXPath xPath
  elements <- tryRepeatedlyTo $ findAnyByXPath' xPath
  traverse_ (expectPresented locator) elements
  pure elements

findAnyByXPathAndProperty :: String -> String -> String -> Check (List Element)
findAnyByXPathAndProperty xPath property expectedValue = tryRepeatedlyTo do
  elements <- findAllByXPath xPath
  values <- traverse (flip getAttribute property) elements
  let matches = map (eq $ Just expectedValue) values
  let elementMatchTuples = zip elements matches
  let matchingElements = map fst $ filter snd elementMatchTuples
  pure matchingElements

findAllByXPathAndProperty :: String -> String -> String -> Check (List Element)
findAllByXPathAndProperty xPath property expectedValue =
  tryRepeatedlyTo $
    passover throwIfEmpty =<< findAnyByXPathAndProperty xPath property expectedValue
  where
  throwIfEmpty xs | length xs > 0 = pure unit
  throwIfEmpty _ = throwNoElementWithPropertyError xPath property expectedValue

findByXPathAndProperty :: String -> String -> String -> Check Element
findByXPathAndProperty xPath property expectedValue =
  tryRepeatedlyTo $
    headOrThrow =<< findAnyByXPathAndProperty xPath property expectedValue
  where
  headOrThrow = maybe' (const throw) pure <<< head
  throw = throwNoElementWithPropertyError property expectedValue xPath

findInputWithPlaceholderAndValue :: String -> String -> Check Element
findInputWithPlaceholderAndValue placeholder value =
  findByXPathAndProperty (XPath.anywhere $ XPath.inputWithPlaceholder placeholder) "value" value

--findSingleGracefully' :: Locator -> Check Element
--findSingleGracefully' loc = findSingle loc <|> (warn *> findExact loc)
--  where
--  warn = warnMsg $
--    "Warning, found more than one element with locator: "
--    ++ showLocator loc
--    ++ ". Are we sure this is the right one?"

