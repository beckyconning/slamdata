module Test.Feature.Scenario where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff.Exception (message)
import Data.Either (Either(..))
import Data.String (joinWith)
import Test.Feature.Log (sectionMsg, warnMsg, errorMsg)
import Test.Feature.Monad (Feature())
import Selenium.Monad (attempt)

scenario
  :: forall eff o
  .  String
  -> Feature eff o Unit
  -> Feature eff o Unit
  -> String
  -> Array String
  -> Feature eff o Unit
  -> Feature eff o Unit
scenario epic before after title knownIssues actions = sectionMsg title' *> before *> actions'
  where
  title' :: String
  title' = epic ++ ": " ++ title

  knownIssuesString :: String
  knownIssuesString = separate $ indent <$> knownIssues

  knownIssuesWarning :: String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" ++ knownIssuesString

  separate :: Array String -> String
  separate = joinWith "\n"

  indent :: String -> String
  indent s = "  " ++ s

  warning :: String -> String
  warning s = "Warning: " ++ s

  warn :: String -> Feature eff o Unit
  warn = warnMsg <<< warning

  fail :: Feature eff o Unit
  fail = errorMsg "Ok despite known issues, if these issues are resolved please remove them"

  actions' :: Feature eff o Unit
  actions' | knownIssues == [] = actions *> after
  actions' = do
    e <- attempt actions
    case e of
      Left e -> warn (message e) *> warn knownIssuesWarning *> after
      Right _ -> after *> fail
