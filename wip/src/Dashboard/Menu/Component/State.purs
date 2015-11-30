module Dashboard.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Char (fromCharCode)
import Data.Key (Platform(), meta, shift, enter, character, printCombination)
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu

import Model.CellType
import Notebook.Component as Notebook
import Dashboard.Menu.Component.Query

type StateP g = HalogenMenu.MenuP Value g

initial :: Platform -> HalogenMenu.Menu Value
initial platform = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Just $ printCombination platform [meta, shift, character (fromCharCode 67)]
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Publish"
          , shortcutLabel: Just $ printCombination platform [meta, character (fromCharCode 80)]
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , shortcutLabel: Just $ printCombination platform [meta, character (fromCharCode 49)]
          , value: notebookQueryToValue $ (Notebook.AddCell Query) unit
          }
        , { label: "Markdown"
          , shortcutLabel: Just $ printCombination platform [meta, character (fromCharCode 50)]
          , value: notebookQueryToValue $ (Notebook.AddCell Markdown) unit
          }
        , { label: "Explore"
          , shortcutLabel: Just $ printCombination platform [meta, character (fromCharCode 51)]
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Search"
          , shortcutLabel: Just $ printCombination platform [meta, character (fromCharCode 52)]
          , value: notebookQueryToValue $ (Notebook.AddCell Search) unit
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , shortcutLabel: Just $ printCombination platform [meta, enter]
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

