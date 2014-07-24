--------------------------------------------------------------------------------
--- This module provides controllers for administrational issues. Currently,
--- this module supports
--- - adding new languages to Smap
--- - adding new systems (associated to existing languages) to Smap.
--- More functionality may be added in the future.
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module AdminController (
  adminController
) where

import KeyDatabase
import Smap
import ExecEnvModel
import UserModel

import ExecEnvsController
import StaticController

import AdminView
import SmapHtml
import Html5

import Alerts
import Authorization
import AuthorizedOperations
import Controllers
import Url

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main administration controller which delegates tasks to other
--- controllers in this module depending on the URL path.
adminController :: Url -> Controller
adminController url@(path,_) =
  case path of
    ["languages","new"] -> showLanguageCreationForm
    ["systems","new"]   -> showSystemCreationForm
    ["systems","list"]  -> listSystemController
    ["systems","edit",syskey] -> validateKeyAndApplyOn
                                     (readSystemKey syskey) url getSystem
                                     editSystemController
    ["systems","delete",syskey] -> validateKeyAndApplyOn
                                     (readSystemKey syskey) url getSystem
                                     confirmDeleteSystemController
    _                   -> showInvalidUrlErrorPage url

--------------------------------------------------------------------------------
-- Controllers
--------------------------------------------------------------------------------

-- Returns a controller that displays a WUI form to create a new language.
showLanguageCreationForm :: Controller
showLanguageCreationForm = 
  checkAuthorization (adminOperation CreateLanguage) $ \_ ->
  return $ languageCreationForm doCreateLanguageCtrl
  where
    doCreateLanguageCtrl = doCreateLanguage
      (const $ showLandingPage,Just langCreationSucceededAlert)
    langCreationSucceededAlert =
     SuccessAlert $ "The new language was successfully added to Smap. Enable "++
      "the creation of new programs of this language by adding an associated "++
      "language implementation system."

-- Returns a controller that displays a WUI form to create a new system.
showSystemCreationForm :: Controller
showSystemCreationForm =
  checkAuthorization (adminOperation CreateSystem) $ \_ ->
  do langs <- getAllLanguages
     return $ systemCreationForm langs doCreateSystemCtrl
  where
    doCreateSystemCtrl = doCreateSystem
      (const $ showLandingPage,Just systemCreationSucceededAlert)
    systemCreationSucceededAlert =
      SuccessAlert $ "The new system was successfully added to Smap. It is "++
         "now available as a new execution system for the choosen language."

--- Shows a form to edit the given System entity.
editSystemController :: System -> Controller
editSystemController systemToEdit =
  checkAuthorization (adminOperation CreateSystem) $ \_ ->
   (do allLanguages <- runQ queryAllLanguages
       langImplLanguage <- runJustT (getLangImplLanguage systemToEdit)
       return
        (editSystemView systemToEdit langImplLanguage allLanguages
          updateSystemController))

--- Persists modifications of a given System entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateSystemController :: System -> Controller
updateSystemController system =
  do transResult <- runT (updateSystem system)
     either (\ _ -> setAlert editSuccess >> listSystemController)
      (\ error -> showStdErrorPage (showTError error)) transResult
 where
  editSuccess = SuccessAlert "System data successfully changed."

--- Lists all System entities with buttons to delete or edit an entity.
listSystemController :: Controller
listSystemController =
  checkAuthorization (adminOperation CreateSystem) $ \_ ->
   (do systems <- runQ queryAllSystems
       return (listSystemView systems))

--- Deletes a given System entity (after asking for confirmation)
--- and proceeds with the list controller.
confirmDeleteSystemController :: System -> Controller
confirmDeleteSystemController system =
  checkAuthorization (adminOperation CreateSystem) $ \_ ->
   confirmController
    (h3 [] [text (concat ["Really delete entity \"",systemName system,"\"?"])])
    (\ ack -> if ack
               then deleteSystemController system
               else listSystemController)

--- Call the next controller after a user confirmation.
--- The Boolean user answer is passed as an argument to the controller.
confirmController :: HtmlExp -> (Bool -> Controller) -> Controller
confirmController question controller = do
  return [panelWith 6
           [text "Confirm Deletion"]
           [question]
           [blueSubmitBtn (\_ -> controller True >>= getForm)  [text "Yes"],
            blueSubmitBtn (\_ -> controller False >>= getForm) [text "No"]]]

--- Deletes a given System entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteSystemController :: System -> Controller
deleteSystemController system =
  checkAuthorization (adminOperation CreateSystem) $ \_ ->
   (do transResult <- runT (deleteSystem system)
       either (\ _ -> setAlert deleteSuccess >> listSystemController)
        (\ error -> showStdErrorPage (showTError error)) transResult)
 where
  deleteSuccess = SuccessAlert $ "System '"++systemName system++"' removed."

--- Gets the associated Language entity for a given System entity.
getLangImplLanguage :: System -> Transaction Language
getLangImplLanguage sLanguage =
  getLanguage (systemLanguageLangImplKey sLanguage)

--------------------------------------------------------------------------------
