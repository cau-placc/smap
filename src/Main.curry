--------------------------------------------------------------------------------
--- The main module of the Smap web application.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version June 2025
--------------------------------------------------------------------------------

module Main ( main )
 where

import HTML.Html5
import System.Controllers
import Config.ControllerMapping
import Controller.Download
import KeyDatabase               ( closeDBHandles )
import System.Routes
import System.Url

--------------------------------------------------------------------------------
-- Main function                                                              --
--------------------------------------------------------------------------------

--- The main function which calls the dispatcher and closes all open database
--- handles (to avoid zombie processes).
main :: IO HtmlPage
main = do
  page <- dispatcher >>= (return $!!) -- strictly evaluate result page
  closeDBHandles                      -- now close all open handles
  return page

--- The dispatcher function. Gets the current URL and the associated controller
--- reference (see `RoutesData`), maps the reference to an actual controller and
--- returns the form.
dispatcher :: IO HtmlPage
dispatcher = do
  url         <- getUrl
  case url of
    ("download":_,_) -> downloadController url
    _ -> do controller  <- getControllerReference url
                           >>= maybe (showInvalidUrlErrorPage url)
                                     (getController url)
            form        <- getPage controller
            return form
          
--------------------------------------------------------------------------------
