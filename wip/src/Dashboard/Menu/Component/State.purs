module Dashboard.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu

import Model.CellType
import Notebook.Component as Notebook
import Dashboard.Menu.Component.Query

type StateP g = HalogenMenu.MenuP Value g

initial :: String -> HalogenMenu.Menu Value
initial modifyerKeyString = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Just $ modifyerKeyString ++ "⇧S"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Publish"
          , shortcutLabel: Just $ modifyerKeyString ++ "P"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , shortcutLabel: Just $ modifyerKeyString ++ "1"
          , value: notebookQueryToValue $ (Notebook.AddCell Query) unit
          }
        , { label: "Markdown"
          , shortcutLabel: Just $ modifyerKeyString ++ "2"
          , value: notebookQueryToValue $ (Notebook.AddCell Markdown) unit
          }
        , { label: "Explore"
          , shortcutLabel: Just $ modifyerKeyString ++ "3"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Search"
          , shortcutLabel: Just $ modifyerKeyString ++ "4"
          , value: notebookQueryToValue $ (Notebook.AddCell Search) unit
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , shortcutLabel: Just $ modifyerKeyString ++ "Enter"
          , value: notebookQueryToValue $ (Notebook.RunActiveCell) unit
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Getting started"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/"
          }
        , { label: "Manual"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/front-end-manual/"
          }
        , { label: "SlamSQL reference"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamsql-reference/"
          }
        , { label: "SlamDown reference"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamdown-reference/"
          }
        , { label: "Cheatsheet"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/support/cheatsheet.pdf"
          }
        , { label: "How to guides"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/how-tos/"
          }
        , { label: "Securing access to SlamData"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation/quick-guide-resources/"
          }
        , { label: "Report a bug"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Bug found"
          }
        , { label: "Request support"
          , shortcutLabel: Nothing
          , value: helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Request help"
          }
        ]
    }
  ]

