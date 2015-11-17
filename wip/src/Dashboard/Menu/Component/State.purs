module Dashboard.Menu.Component.State where

import Prelude

import Data.Functor.Coproduct (Coproduct(..), left, right)

import Halogen.Menu.Model as HalogenMenu
import Halogen.Query (action)
import Halogen.Menu.Component as HalogenMenu

import Model.CellType
import Notebook.Component as Notebook
import Dashboard.Dialog.Component as Dialog
import Dashboard.Menu.Component.Query

type StateP g = HalogenMenu.MenuP Value g

initial :: HalogenMenu.Menu Value
initial = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Delete"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Publish"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , value: notebookQueryToValue $ (Notebook.AddCell Query) unit
          }
        , { label: "Markdown"
          , value: notebookQueryToValue $ (Notebook.AddCell Markdown) unit
          }
        , { label: "Explore"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        , { label: "Search"
          , value: notebookQueryToValue $ (Notebook.AddCell Search) unit
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , value: notebookQueryToValue $ (Notebook.RunActiveCell) unit
          }
        , { label: "Delete"
          , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Tutorial"
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation"
          }
        , { label: "SQL Tutorial"
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation"
          }
        , { label: "SQL Reference"
          , value: helpURIToValue $ HelpURI "http://slamdata.com/documentation"
          }
        , { label: "Report bug"
          , value: helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Bug found"
          }
        , { label: "Request support"
          , value: helpURIToValue $ HelpURI "mailto:support@slamdata.com?subject=Request help"
          }
        ]
    }
  ]

