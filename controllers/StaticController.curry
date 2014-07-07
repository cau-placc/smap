--------------------------------------------------------------------------------
--- This module provides controllers that display all kinds of static pages
--- (like the landing page, the help page or the abput page). 
--- 
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

import Controllers
import StaticView
import Url

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
showHelpPage = return helpPage

-- Returns a controller that displays the about page of this application.
showAboutPage :: Controller
showAboutPage = return aboutPage

--------------------------------------------------------------------------------