--------------------------------------------------------------------------------
--- The main module of this Spicey application.
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module Main (
  main
) where

import Html5
import Controllers
import ControllerMapping
import DownloadController
import Routes
import Url

--------------------------------------------------------------------------------
-- Main function                                                              --
--------------------------------------------------------------------------------

--- The main function! Calls the dispatcher.
main :: IO HtmlForm
main = dispatcher

--- The dispatcher function. Gets the current URL and the associated controller
--- reference (see `RoutesData`), maps the reference to an actual controller and
--- returns the form.
dispatcher :: IO HtmlForm
dispatcher = do
  url         <- getUrl
  case url of
    ("download":_,_) -> downloadController url
    _ -> do controller  <- getControllerReference url
                           >>= maybe (showInvalidUrlErrorPage url)
                                     (getController url)
            form        <- getForm controller
            return form
          
--------------------------------------------------------------------------------
