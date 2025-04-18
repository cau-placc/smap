--------------------------------------------------------------------------------
--- The main module of this Spicey application.
--- @author Lasse Kristopher Meyer
--- @version October 2022
--------------------------------------------------------------------------------

module Main (
  main
) where

import HTML.Html5
import System.Controllers
import Config.ControllerMapping
import Controller.Download
import System.Routes
import System.Url

--------------------------------------------------------------------------------
-- Main function                                                              --
--------------------------------------------------------------------------------

--- The main function! Calls the dispatcher.
main :: IO HtmlPage
main = dispatcher

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
