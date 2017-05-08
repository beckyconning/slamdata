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

module Test.SlamData.Feature.Test.Markdown where

import Prelude
import Data.String (joinWith)
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Expectations as Expect
import Test.Feature.Scenario (scenario)

mdScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
mdScenario =
  scenario
    "Markdown"
    (Interact.createWorkspaceInTestFolder "Markdown")
    (Interact.deleteFileInTestFolder "Markdown.slam"
       *> Interact.browseRootFolder)

test ∷ SlamFeature Unit
test = do
  mdScenario "Provide and play markdown" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard
      $ joinWith "\n\n"
          [ "discipline = __"
          , "sport = __ (Bobsleigh)"
          , "age = #__"
          , "year = #__ (2002)"
          , "startDate = __ - __ - __"
          , "finishDate = __ - __ - __ (2002-06-06)"
          , "startTime = __ : __"
          , "finishTime = __ : __ (20:39)"
          , "event = {1000m, 1500m, 3000m} (1500m)"
          , "gender = []M []W []X"
          , "color = [x]Red []Green [x]Blue"
          , "type = (x)Gold ()Silver ()Bronze"
          ]
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.fieldInLastMdCard "discipline" "text" ""
    Expect.fieldInLastMdCard "sport" "text" "Bobsleigh"
    Expect.fieldInLastMdCard "age" "number" ""
    Expect.fieldInLastMdCard "year" "number" "2002"
    Expect.fieldInLastMdCard "startDate" "date" ""
    Expect.fieldInLastMdCard "finishDate" "date" "2002-06-06"
    Expect.fieldInLastMdCard "startTime" "time" ""
    Expect.fieldInLastMdCard "finishTime" "time" "20:39"
    Expect.labelInLastMdCard "event"
    Expect.dropdownInLastMdCard "1500m" ["1500m", "1000m", "3000m"]
    Expect.labelInLastMdCard "gender"
    Expect.checkableFieldInLastMdCard "X" "checkbox" false
    Expect.checkableFieldInLastMdCard "W" "checkbox" false
    Expect.checkableFieldInLastMdCard "M" "checkbox" false
    Expect.labelInLastMdCard "color"
    Expect.checkableFieldInLastMdCard "Red" "checkbox" true
    Expect.checkableFieldInLastMdCard "Green" "checkbox" false
    Expect.checkableFieldInLastMdCard "Blue" "checkbox" true
    Expect.labelInLastMdCard "type"
    Expect.checkableFieldInLastMdCard "Gold" "radio" true
    Expect.checkableFieldInLastMdCard "Silver" "radio" false
    Expect.checkableFieldInLastMdCard "Bronze" "radio" false

    successMsg "Ok, succesfully provided and played markdown."

  mdScenario "Change and play markdown" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard "discipline = __"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Interact.accessPreviousCardInLastDeck
    Interact.provideMdInLastMdCard "sport =  __ (Bobsleigh)"
    Interact.runQuery
    Interact.accessNextCardInLastDeck

    Expect.fieldInLastMdCard "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

  mdScenario "Provide and play markdown with evaluated content" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.fieldInLastMdCard "discipline" "text" "Figure skating"
    Expect.fieldInLastMdCard "year" "text" "1924"
    Expect.labelInLastMdCard "country"
    Expect.dropdownInLastMdCard "AUT"
      [ "LAT"
      , "CZE"
      , "UKR"
      , "SLO"
      , "RUS"
      , "SVK"
      , "KAZ"
      , "AUS"
      , "LUX"
      , "UZB"
      , "EUN"
      , "DEN"
      , "CHN"
      , "ROU"
      , "GDR"
      , "PRK"
      , "CRO"
      , "URS"
      , "BLR"
      , "BUL"
      , "POL"
      , "EUA"
      , "KOR"
      , "NED"
      , "ITA"
      , "FRG"
      , "EST"
      , "SWE"
      , "GBR"
      , "TCH"
      , "BEL"
      , "FIN"
      , "USA"
      , "YUG"
      , "SUI"
      , "LIE"
      , "CAN"
      , "JPN"
      , "HUN"
      , "GER"
      , "NOR"
      , "NZL"
      , "FRA"
      , "AUT"
      , "ESP"
      ]
    Expect.labelInLastMdCard "gender"
    Expect.checkableFieldInLastMdCard "X" "checkbox" false
    Expect.checkableFieldInLastMdCard "W" "checkbox" true
    Expect.checkableFieldInLastMdCard "M" "checkbox" false
    Expect.labelInLastMdCard "type"
    Expect.checkableFieldInLastMdCard "Gold" "radio" false
    Expect.checkableFieldInLastMdCard "Silver" "radio" true
    Expect.checkableFieldInLastMdCard "Bronze" "radio" false
    successMsg "Ok, successfully provided and played markdown with evaluated content"

  mdScenario "Filter query results with default field values" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.displayMarkdownCardPresented
    Interact.accessNextCardInLastDeck
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "SELECT * FROM `/test-mount/testDb/olympics` WHERE discipline = :discipline AND type != :type AND gender IN :gender AND year > :year AND country = :country"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "discipline"
    Interact.addColumn "country"
    Interact.addColumn "gender"
    Interact.addColumn "year"
    Interact.addColumn "type"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToEq 2 "discipline" "Figure skating"
    Expect.cellsInTableColumnInLastCardToEq 2 "country" "AUT"
    Expect.cellsInTableColumnInLastCardToEq 2 "gender" "W"
    Expect.cellsInTableColumnInLastCardToBeGT 2 "year" "1924"
    Expect.cellsInTableColumnInLastCardToNotEq 2 "type" "Silver"
    successMsg "Ok, Filtered query results with fields"

  mdScenario "Filter query results by changing field values" ["https://github.com/slamdata/slamdata/issues/1512"] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.displayMarkdownCardPresented
    Interact.accessNextCardInLastDeck
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "SELECT * FROM `/test-mount/testDb/olympics` WHERE discipline = :discipline AND type != :type AND gender IN :gender AND year > :year AND country = :country"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard

    Interact.addColumn "discipline"
    Interact.addColumn "gender"
    Interact.addColumn "year"
    Interact.addColumn "type"
    Interact.addColumn "country"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Interact.accessPreviousCardInLastDeck
    Interact.accessPreviousCardInLastDeck
    Interact.accessPreviousCardInLastDeck
    Interact.provideFieldValueInLastDeck "discipline" "Luge"
    Interact.provideFieldValueInLastDeck "year" "1950"
    Interact.uncheckFieldInLastDeck "W"
    Interact.checkFieldInLastDeck "X"
    Interact.checkFieldInLastDeck "M"
    Interact.pushRadioButtonInLastDeck "Gold"
    Interact.selectFromDropdownInLastDeck "country" "GDR"
    Interact.accessNextCardInLastDeck
    Interact.accessNextCardInLastDeck
    Interact.accessNextCardInLastDeck
    Expect.cellsInTableColumnInLastCardToEq 8 "discipline" "Luge"
    Expect.cellsInTableColumnInLastCardToEqOneOf 8 "gender" ["M", "X"]
    Expect.cellsInTableColumnInLastCardToBeGT 8 "year" "1950"
    Expect.cellsInTableColumnInLastCardToNotEq 8 "type" "Gold"
    successMsg "Ok, Filtered query results by changing field values"

  mdScenario "Markdown chaining" [] do
    let
      states = ["MA", "MN", "SC", "RI", "OK", "SD", "NH", "ME", "NJ", "VT", "CA", "CT", "CO", "PA", "LA", "NY", "DE", "DC", "OR", "MD", "VA", "WV", "NC", "NE", "GA", "TX", "FL", "AL", "TN", "MS", "AK", "KY", "AZ", "OH", "IN", "MI", "IA", "KS", "WI", "ND", "MT", "IL", "UT", "MO", "AR", "NM", "WY", "ID", "NV", "HI", "WA"]
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard $
      "state = {!``select distinct state from `/test-mount/testDb/zips` order by asc``}"
      <> "(!``select state from `/test-mount/testDb/zips` limit 1 ``)"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.dropdownInLastMdCard "MA" states
    Interact.accessNextCardInLastDeck
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard $
      "city = ___"
      <> "(!``select city from `/test-mount/testDb/zips` where state = :state order by asc limit 1``)"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.fieldInLastMdCard "city" "text" "ABINGTON"
    Interact.accessPreviousCardInLastDeck
    Interact.accessPreviousCardInLastDeck
    Interact.selectFromDropdownInLastDeck "state" "RI"
    Interact.accessNextCardInLastDeck
    Interact.accessNextCardInLastDeck
    Expect.fieldInLastMdCard "city" "text" "ASHAWAY"
    successMsg "Ok, variables from md card could be used in md card"
