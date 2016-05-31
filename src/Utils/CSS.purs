{-
Copyright 2016 SlamData, Inc.

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

module Utils.CSS where

import Prelude
import CSS (CSS)
import CSS.String (fromString)
import CSS.Stylesheet (key)

transition :: String -> CSS
transition = key (fromString "transition")

calc :: String -> String
calc s = "calc(" ++ s ++ ")"

width :: String -> CSS
width = key (fromString "width")

left :: String -> CSS
left = key (fromString "left")

transform :: String -> CSS
transform = key (fromString "transform")

translate3d :: String -> String -> String -> String
translate3d x y z = "translate3d(" ++ x ++ "," ++ y ++ "," ++ z ++ ")"

visibility :: String -> CSS
visibility = key (fromString "visibility")

position :: String -> CSS
position = key (fromString "position")

marginRight :: String -> CSS
marginRight = key (fromString "margin-right")
