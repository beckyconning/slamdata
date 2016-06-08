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

module Test.SlamData.Feature.Test.Dashboard where

import Prelude

import Data.String as Str
import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, makeEChartsPresentConfig)
import Selenium.Monad (script, tryRepeatedlyTo)

dashboardScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
dashboardScenario =
  scenario
    "Dashboard"
    (Interact.createWorkspaceInTestFolder "Dashboard")
    (Interact.deleteFileInTestFolder "Dashboard.slam")

test :: SlamFeature Unit
test =
  dashboardScenario "Make simple logo and chart dashboard" [] do
    makeEChartsPresentConfig
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard
      "## 🇺🇸  TEAM USA 🇺🇸"
    Interact.accessNextCardInLastDeck
    Interact.insertFormCardInLastDeck

    Interact.wrapLastDeck
    Interact.moveLastDeckToLeft
    Interact.resizeLastDeckAllTheWayToTheRight

    Interact.insertNewDeckInLastBoardCard
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      """select count(*), year, type from `/test-mount/testDb/olympics` where country = "USA" group by year, type"""
    Interact.accessNextCardInLastDeck
    Interact.insertVisualizeCardInLastDeck
    Interact.switchToLineChart
    Interact.provideDimensionForLastVisualizeCard ".year"
    Expect.measureInLastVisualizeCard ".0"
    Interact.provideSeriesForLastVizualizeCard ".type"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck

    Interact.moveLastDeckToLeft
    Interact.resizeLastDeckAllTheWayToTheRight

    Expect.nthDeckToBeAboveMthDeck 2 3
    Expect.nthDeckToBeFullWidth 2
    Expect.nthDeckToBeFullWidth 3
    Expect.textInFormCard "🇺🇸  TEAM USA 🇺🇸"
    Expect.lastEChartOptions
      """{"color":["#93A9A6","#CDA71F","#EB6F76","#66B35B","#BD97E2","#5B5925","#884A6F","#51B3F6","#CCA067","#398465","#3C607A","#81463C","#B65F33","#9AAE31","#CE8B9E","#6C6356","#95A779","#44AECB","#E987C2","#8A7EA0","#3D6C2F","#40B994","#87984A","#C1A088","#9B6E2D","#428C8D","#B8766A","#EB8666","#DF883E","#BB6273","#C994BF","#929DE0","#7BA5CB","#AE9093","#66557B","#936370","#3C6D64","#84693F","#C19744","#E77AA1","#5D555F","#4078A2","#3FAFAF","#698B99","#486A4C","#7EA48B","#B57E57","#8C72AA","#609050","#56B379","#489F8B","#714D4A","#9A8867","#93B66B","#7DA93F","#877424","#C75D56","#B774AC","#7B7A3E","#73581C","#398EA3","#964734","#DF8D89","#AF97CC","#96951F","#A37791","#7C4D2E","#78865F","#216B74","#935524","#6FAFB6","#75AB76","#A48B50","#D28DD0","#BE9AAF","#AD8D22","#D89576","#964860","#9B9A61","#4DAADB","#A9628D","#98943B","#486366","#6D7E2B","#CF9A2F","#827A8B","#876A69","#495F23","#677F45","#805845","#A2544D","#8C5157","#6B6C9E","#236443","#919B82","#CC8E55","#3E8555","#A08A7A","#767870","#6D9643","#87658F","#3BB069","#6A5D42","#586249","#1F7769","#6DAF8E","#8FA7BE","#B7A82C","#A09DA0","#7D8AA6","#78A3E0","#719186","#765771","#A37EA7","#8E8CBC","#A76840","#49934B","#A27C62","#3DA27B","#A9AC53","#6685B4","#5F728A","#CB6B4A","#9F8DD3","#B7A66E","#A998B3","#85A362","#595146"],"series":[{"name":"Bronze","type":"line","data":[17,15,14,17,22,23,24,27,31,32,35,37,38,39,46,56,61,68,78,84],"yAxisIndex":0},{"name":"Gold","type":"line","data":[16,14,14,17,22,22,25,28,34,35,36,37,38,39,46,57,61,69,80,84],"yAxisIndex":0},{"name":"Silver","type":"line","data":[16,12,14,17,24,22,23,26,38,39,34,37,39,39,46,58,61,68,76,84],"yAxisIndex":0}],"tooltip":{"trigger":"item"},"legend":{"data":[{"name":"Bronze"},{"name":"Gold"},{"name":"Silver"}]},"grid":{"y2":"15.0%"},"xAxis":{"type":"category","axisTick":{},"axisLabel":{"textStyle":{"fontSize":12},"rotate":30},"data":["1924.0","1928.0","1932.0","1936.0","1948.0","1952.0","1956.0","1960.0","1964.0","1968.0","1972.0","1976.0","1980.0","1984.0","1988.0","1992.0","1994.0","1998.0","2002.0","2006.0"]},"yAxis":{"type":"value"}}"""

    successMsg "Successfully created simple logo and chart dashboard"
