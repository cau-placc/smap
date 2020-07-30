--------------------------------------------------------------------------------
--- This module provides controllers for administrational issues. Currently,
--- this module supports
--- - adding new languages to Smap
--- - adding new systems (associated to existing languages) to Smap.
--- More functionality may be added in the future.
---
--- @author Lasse Kristopher Meyer
--- @version July 2020
--------------------------------------------------------------------------------

module Controller.Admin (
  adminController, langCreateForm, sysCreateForm, sysEditForm
) where

import Global

import KeyDatabase
import Model.Smap
import Model.ExecEnv
import Model.User

import Controller.ExecEnvs
import Controller.Static

import View.Admin
import System.SmapHtml
import HTML.Html5
import WUI

import System.Alerts
import System.Authorization
import System.AuthorizedOperations
import System.Controllers
import System.Session
import System.Url

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main administration controller which delegates tasks to other
--- controllers in this module depending on the URL path.
adminController :: Url -> Controller
adminController url@(path,_) = case path of
  ["languages","new"]          -> showLanguageCreationForm
  ["systems","new"]            -> showSystemCreationForm
  ["systems","list"]           -> listSystemController
  ["systems","edit",syskey]    -> sysControllerOn syskey editSystemController
  ["systems","delete",syskey]  -> sysControllerOn syskey deleteSystemController
  ["systems","destroy",syskey] -> sysControllerOn syskey destroySystemController
  ["users","list"]             -> listUserController
  _                            -> showInvalidUrlErrorPage url
 where
  sysControllerOn syskey =
    validateKeyAndApplyOn (readSystemKey syskey) url getSystem

--------------------------------------------------------------------------------
-- Controllers
--------------------------------------------------------------------------------

-- Returns a controller that displays a WUI form to create a new language.
showLanguageCreationForm :: Controller
showLanguageCreationForm = 
  checkAuthorization (adminOperation CreateLanguage) $ \_ -> do
    setWuiStore langCreateStore ("","","")
    return [formExp langCreateForm]

--- A WUI form to create a new language supported by Smap.
--- The default values for the fields are stored in 'langCreateStore'.
langCreateForm :: HtmlFormDef (WuiStore (String,String,String))
langCreateForm =
  wui2FormDef "Controller.Admin.langCreateForm" langCreateStore
   wLanguage
   (\langdata ->
     checkAuthorization (adminOperation CreateLanguage) $ \_ ->
       doCreateLanguageCtrl langdata)
   languageCreationRendering
 where
  doCreateLanguageCtrl = doCreateLanguage
    (const $ showLandingPage, Just langCreationSucceededAlert)

  langCreationSucceededAlert =
    SuccessAlert $ "The new language was successfully added to Smap. Enable "++
      "the creation of new programs of this language by adding an associated "++
      "language implementation system."


--- The data stored for executing the "langCreate" WUI form.
langCreateStore :: Global (SessionStore (WuiStore (String,String,String)))
langCreateStore =
  global emptySessionStore (Persistent (inDataDir "langCreateStore"))

------------------------------------------------------------------------------
-- Returns a controller that displays a WUI form to create a new system.
showSystemCreationForm :: Controller
showSystemCreationForm =
  checkAuthorization (adminOperation CreateSystem) $ \_ -> do
    langs <- getAllLanguages
    setParWuiStore sysCreateStore langs ("","",head langs)
    return [formExp sysCreateForm]

--- A WUI form to create a new system supported by Smap.
--- The default values for the fields are stored in 'sysCreateStore'.
sysCreateForm :: HtmlFormDef ([Language], WuiStore (String,String,Language))
sysCreateForm =
  pwui2FormDef "Controller.Admin.sysCreateForm" sysCreateStore
   (\langs -> wSystem langs)
   (\_ sysdata ->
     checkAuthorization (adminOperation CreateSystem) $ \_ ->
       doCreateSystemCtrl sysdata)
   (\_ -> systemCreationRendering)
 where
  doCreateSystemCtrl = doCreateSystem
    (const $ showLandingPage, Just systemCreationSucceededAlert)

  systemCreationSucceededAlert =
    SuccessAlert $ "The new system was successfully added to Smap. It is "++
      "now available as a new execution system for the choosen language."

--- The data stored for executing the "sysCreate" WUI form.
sysCreateStore ::
  Global (SessionStore ([Language], WuiStore (String,String,Language)))
sysCreateStore =
  global emptySessionStore (Persistent (inDataDir "sysCreateStore"))
                      
------------------------------------------------------------------------------
--- Shows a form to edit the given System entity.
editSystemController :: System -> Controller
editSystemController systemToEdit =
  checkAuthorization (adminOperation CreateSystem) $ \_ -> do
    allLanguages <- runQ queryAllLanguages
    langImplLanguage <- runJustT (getLangImplLanguage systemToEdit)
    setParWuiStore sysEditStore (systemToEdit,langImplLanguage,allLanguages)
                                systemToEdit
    return [formExp sysEditForm]

--- A WUI form to create a new system supported by Smap.
--- The default values for the fields are stored in 'sysEditStore'.
sysEditForm :: HtmlFormDef ((System,Language,[Language]), WuiStore System)
sysEditForm =
  pwui2FormDef "Controller.Admin.sysEditForm" sysEditStore
   (\ (system, relatedLanguage, possibleLanguages) ->
        wSystemType system relatedLanguage possibleLanguages)
   (\_ sysdata ->
     checkAuthorization (adminOperation CreateSystem) $ \_ ->
       updateSystemController sysdata)
   (\_ -> editSystemRendering)

--- The data stored for executing the "sysEdit" WUI form.
sysEditStore ::
  Global (SessionStore ((System,Language,[Language]), WuiStore System))
sysEditStore =
  global emptySessionStore (Persistent (inDataDir "sysEditStore"))

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

------------------------------------------------------------------------------
--- Lists all System entities with buttons to delete or edit an entity.
listSystemController :: Controller
listSystemController =
  checkAuthorization (adminOperation CreateSystem) $ \_ -> do
    systems <- runQ queryAllSystems
    return $ listSystemView systems

--- Deletes a given System entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteSystemController :: System -> Controller
deleteSystemController system =
  checkAuthorization (adminOperation CreateSystem) $ \_ -> do
   (upath,_) <- getUrl
   case upath of
     [sys,_,sysid] -> do
       let question = h3 [] [text (concat ["Really delete entity \"",
                                           systemName system, "\"?"])]
       return
         [panelWith 6
           [text "Confirm Deletion"]
           [question]
           [blueLinkBtn (showUrl ([sys,"destroy",sysid],[])) [htxt "Yes"],
            blueLinkBtn (showUrl ([sys,"list"],[]))          [htxt "No"]]]

--- Deletes a given System entity and proceeds with the list controller.
destroySystemController :: System -> Controller
destroySystemController system =
  checkAuthorization (adminOperation CreateSystem) $ \_ -> do
    transResult <- runT (deleteSystem system)
    either (\ _ -> setAlert deleteSuccess >> listSystemController)
           (\ error -> showStdErrorPage (showTError error))
           transResult
 where
  deleteSuccess = SuccessAlert $ "System '" ++ systemName system ++ "' removed."

--- Gets the associated Language entity for a given System entity.
getLangImplLanguage :: System -> Transaction Language
getLangImplLanguage sLanguage =
  getLanguage (systemLanguageLangImplKey sLanguage)

------------------------------------------------------------------------------
--- Lists all User entities.
listUserController :: Controller
listUserController =
  checkAuthorization (adminOperation EditUser) $ \_ -> do
    users <- runQ queryAllUsers
    return $ listUserView users

------------------------------------------------------------------------------
