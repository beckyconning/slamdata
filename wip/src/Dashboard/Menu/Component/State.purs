module Dashboard.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (index)
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu

import Model.CellType (CellType(..))
import Notebook.Component as Notebook
import Dashboard.Component.State (KeyboardShortcut())
import Dashboard.Menu.Component.Query

type StateP g = HalogenMenu.MenuP (Maybe Value) g

make :: (Array KeyboardShortcut) -> HalogenMenu.Menu (Maybe Value)
make shortcuts = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Nothing
          , value: Just $ notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: Nothing
          }
        , { label: "Publish"
          , shortcutLabel: Just $ ""
          , value: Nothing
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , shortcutLabel: index shortcuts 0 >>= _.label
          , value: index shortcuts 0 >>= _.value >>> pure
          }
        , { label: "Markdown"
          , shortcutLabel: index shortcuts 1 >>= _.label
          , value: index shortcuts 1 >>= _.value >>> pure
          }
        , { label: "Explore"
          , shortcutLabel: index shortcuts 2 >>= _.label
          , value: index shortcuts 2 >>= _.value >>> pure
          }
        , { label: "Search"
          , shortcutLabel: index shortcuts 3 >>= _.label
          , value: index shortcuts 3 >>= _.value >>> pure
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , shortcutLabel: index shortcuts 4 >>= _.label
          , value: index shortcuts 4 >>= _.value >>> pure
          }
        , { label: "Delete"
          , shortcutLabel: Nothing
          , value: Nothing
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Getting started"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/"
          }
        , { label: "Manual"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/front-end-manual/"
          }
        , { label: "SlamSQL reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamsql-reference/"
          }
        , { label: "SlamDown reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/slamdown-reference/"
          }
        , { label: "Cheatsheet"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/support/cheatsheet.pdf"
          }
        , { label: "How to guides"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/how-tos/"
          }
        , { label: "Securing access to SlamData"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/documentation/quick-guide-resources/"
          }
        , { label: "Report a bug"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Bug found"
          }
        , { label: "Request support"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Request help"
          }
        ]
    }
  ]

