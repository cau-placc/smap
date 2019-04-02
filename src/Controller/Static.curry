--------------------------------------------------------------------------------
--- This module provides controllers that display all kinds of static pages
--- (like the landing page, the help page or the about page). 
--- 
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2014
--------------------------------------------------------------------------------

module Controller.Static where

import System.Controllers
import View.Static
import System.Url

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main static controller which delegates tasks to other controllers in
--- this module depending on the URL path.
--- @param url - the current URL
staticController :: Url -> Controller
staticController url@(path,_) =
  case path of
    [""]      -> showLandingPage
    ["help"]  -> showHelpPage
    ["about"] -> showAboutPage
    _         -> showInvalidUrlErrorPage url

--------------------------------------------------------------------------------
-- Controllers                                                                --
--------------------------------------------------------------------------------

-- Returns a controller that displays the landing page of this application.
showLandingPage :: Controller
showLandingPage = return landingPage

-- Returns a controller that displays the help page of this application.
showHelpPage :: Controller
showHelpPage = do
  helpTexts <- mapIO readHelpFile helpTextFiles
  return $ helpPage helpTexts

-- Returns a controller that displays the about page of this application.
showAboutPage :: Controller
showAboutPage = return aboutPage

--------------------------------------------------------------------------------
-- Reads a help file and returns the first line (unique help key),
-- second line (title), and remaining lines:
readHelpFile :: String -> IO (String,String,String)
readHelpFile fname = do
  cnt <- readFile ("static/"++fname)
  let (key,wokey) = break (=='\n') cnt
      (title,txt) = break (=='\n') (tail wokey)
  return (key,title,tail txt)

-- The list of help text files (stored in public/static):
helpTextFiles =
  map (\t -> "help"++t++".txt")
      ["Smap","SmapIE","Browser","SignIn","Languages"]
