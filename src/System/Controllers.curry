--------------------------------------------------------------------------------
--- This module provides general controllers (e.g. controllers for displaying
--- errors), controller related operations on data and HTML form construction.
--- This module should be imported by all controller modules, given that it
--- also exports the general type synonym for controllers.
---
--- @author Lasse Kristopher Meyer
--- @version November 2018
--------------------------------------------------------------------------------

module System.Controllers (
  Controller,
  showErrorPage,showStdErrorPage,showAccessDeniedErrorPage,
  showInvalidUrlErrorPage,showTransactionErrorPage,
  showNotYetImplementedErrorPage,stdErrorPageTitle,
  validateKeyAndApply,validateKeyAndApplyOn,next,nextFor,
  getForm
) where

import KeyDatabase
import Prelude hiding (div)

import System.Alerts
import System.Authentication
import Model.ExecEnv
import System.Session
import System.SmapHtml
import System.Url


--------------------------------------------------------------------------------
-- Controller type                                                            --
--------------------------------------------------------------------------------

--- A controller contains the application logic and reacts to user actions.
--- Controllers always return a corresponding view as an I/O action,
type Controller = IO [HtmlExp]

--------------------------------------------------------------------------------
-- Controllers for displaying errors                                          --
--------------------------------------------------------------------------------

--- Returns a controller that displays the general error page with a given
--- title, a controller error message and an optional internal error message 
--- (e.g. transaction error messages).
--- @param title         - the title of the error page panel
--- @param controllerMsg - an error message specified in the calling controller
--- @param mInternalMsg  - an error message from an internal operation
showErrorPage :: String -> String -> Maybe String -> Controller
showErrorPage title controllerMsg mInternalMsg = 
  return [renderErrorPage (title,controllerMsg,mInternalMsg)]

--- Returns a controller that displays a standard error page with a given
--- controller error message.
--- @param controllerMsg - an error message specified in the calling controller
showStdErrorPage :: String -> Controller
showStdErrorPage controllerMsg =
  showErrorPage stdErrorPageTitle controllerMsg Nothing

--- Returns a controller that displays a standard error page for denied access
--- requests.
--- @param reason - the reason for the denied access
showAccessDeniedErrorPage :: String -> Controller
showAccessDeniedErrorPage reason =
  showErrorPage "Sorry, access denied!" reason Nothing

--- Returns a controller that displays a standard error page for invalid URL
--- requests.
--- @param url - the url that caused the error
showInvalidUrlErrorPage :: Url -> Controller
showInvalidUrlErrorPage url = 
  showErrorPage stdErrorPageTitle invalidUrlMsg Nothing
  where 
    invalidUrlMsg =
      "The requested URL <code>\""++(drop 1 $ showPath url)++"\"</code> was n"++
      "ot found on this server."

--- Returns a controller that displays a standard error page for internal
--- transaction errors.
--- @param controllerMsg  - an error message specified in the calling controller
--- @param transactionErr - the error from the failed transaction
showTransactionErrorPage :: String -> TError -> Controller
showTransactionErrorPage controllerMsg =
  showErrorPage stdErrorPageTitle controllerMsg . Just . showTError

--- Returns a controller that displays a standard error page for not yet
--- implemented features.
showNotYetImplementedErrorPage :: Controller
showNotYetImplementedErrorPage =
  showErrorPage stdErrorPageTitle notYetImplementedMsg Nothing
  where 
    notYetImplementedMsg =
      "Sorry, this function is not yet implemented."

-- The standard title for the error page panel.
stdErrorPageTitle :: String
stdErrorPageTitle = "Oops, an error occured!"

-- The standard rendering of error pages.
-- @param errorPageData - title, controller error and internal error
renderErrorPage :: (String,String,Maybe String) -> HtmlExp
renderErrorPage (title,controllerMsg,mInternalMsg) =
  container
    [row
      [div [classA "col-xs-6 col-xs-offset-3"]
        [div [classA "panel panel-danger"]
          [div [classA "panel-heading"]
            [h3 [classA "panel-title"] 
              [glyphicon "warning-sign",text $ ' ':title]]
          ,div [classA "panel-body"] $ 
            [label [] 
              [glyphicon "exclamation-sign"
              ,text " Controller error message:"]
            ,br []
            ,text controllerMsg]
          ++maybe [] (\internalMsg -> 
              [br []
              ,br []
              ,label [] 
                [glyphicon "cog"
                ,text " Internal error message:"]
              ,br []
              ,div [classA "well"]
                [code [] [text internalMsg]]])
            mInternalMsg
          ++[hr []
            ,greyLinkBtn landingPageUrl 
              [glyphicon "home",text " Back to home"]]]]]]
  `addId` "error-page"

--------------------------------------------------------------------------------
-- Applying and calling controllers                                           --
--------------------------------------------------------------------------------

--- Checks if the result of a key reading function is a valid key and applies a
--- controller on it. Shows the invalid URL error page if no key is given.
--- @param mKey              - the result of the key reading function
--- @param url               - the url that contains the key
--- @param applyController   - the controller which can be applied on the key
validateKeyAndApply :: Maybe key -> Url -> (key -> Controller) -> Controller
validateKeyAndApply mKey url applyController =
  maybe (showInvalidUrlErrorPage url)
        applyController
        mKey

--- Reads an entity for a given key and applies a controller to it.
--- Checks if the result of a key reading function is a valid key and applies a
--- controller on the corresponding entity.
--- Shows the invalid URL error page if no key is given.
--- @param mKey              - the result of the key reading function
--- @param url               - the url that contains the key
--- @param applyController   - the controller which can be applied on the key
validateKeyAndApplyOn :: Maybe key -> Url -> (key -> Transaction en)
                      -> (en -> Controller) -> Controller
validateKeyAndApplyOn mKey url getentity applyController =
  maybe (showInvalidUrlErrorPage url)
        (\key -> runJustT (getentity key) >>= applyController)
        mKey

--- @param controller - the controller that will be executed
next :: Controller -> IO HtmlForm
next controller =
  do view <- controller
     getForm view

--- Runs the action of a controller which takes an additional argument and
--- delivers the HTML form.
--- @param controller - the controller that will be executed
--- @param arg        - the additional argument 
nextFor :: (a -> Controller) -> a -> IO HtmlForm
nextFor controller arg =
  do view <- controller arg
     getForm view

--------------------------------------------------------------------------------
-- Building the HTML form                                                     --
--------------------------------------------------------------------------------

--- Builds the complete HTML form from the view returned by a controller. Adds
--- form parameters and the basic layout (including the navigation bar and
--- sticky footer).
--- @param view - the view returned by the last active controller
getForm :: [HtmlExp] -> IO HtmlForm
getForm view = 
  do cookie <- sessionCookie
     body   <- addLayoutToView
     langs  <- getAllLanguages
     return $ HtmlForm "Smap"
                ([viewportMetaTag,cookie,favicon,MultipleHandlers]
                ++(jsHeadIncludes $ map languageName langs)
                ++cssIncludes)
                (body++jsBodyIncludes)
  where
    addLayoutToView =
      do url        <- getUrl
         langs      <- getAllLanguages
         mAuthNData <- getSessionAuthNData
         mAlert     <- getAlert
         return $ 
           [wrap styleAttrs $
             [renderNavbar url langs mAuthNData]
           ++[maybe empty renderAlert mAlert]
           ++view]++
           [stickyFooter]
    styleAttrs = case view of -- adds styles for specific pages
      [HtmlStruct "input" [("type","hidden"),("value","smap-ie")] [],_] ->
        [style "height: 100%;"]
      _ -> []

--------------------------------------------------------------------------------