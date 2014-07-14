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

import ExecEnvModel
import UserModel

import ExecEnvsController
import StaticController

import AdminView

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
      (SuccessAlert,"The new language was successfully added to Smap. Enable "++
      "the creation of new programs of this language by adding an associated "++
      "language implementation system.")

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
      (SuccessAlert,"The new system was successfully added to Smap. It is now"++
      " available as a new execution system for the choosen language.")

--------------------------------------------------------------------------------
