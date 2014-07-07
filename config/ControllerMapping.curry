--------------------------------------------------------------------------------
--- This module provides the mapping from controller references associated to
--- URLs in the module `RouesData` into actual controllers.
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module ControllerMapping (
  getController
) where

import RoutesData

import AdminController
import AuthNController
import BrowserController
import SmapIEController
import StaticController

import Controllers
import Url

--------------------------------------------------------------------------------
-- Map controller references to controllers                                   --
--------------------------------------------------------------------------------

--- Maps the controller references associated to URLs in the module `RoutesData`
--- into actual controllers and applies them on the current URL for further
--- controller-specific URL mapping.
--- @param url     - the current URL
--- @param ctrlRef - the controller reference from the module `RoutesData`
getController :: Url -> ControllerReference -> Controller
getController url ctrlRef =
  case ctrlRef of
    AdminController   -> adminController   url
    AuthNController   -> authNController   url
    BrowserController -> browserController url
    SmapIEController  -> smapIEController  url
    StaticController  -> staticController  url
    _                 -> showInvalidUrlErrorPage url

--------------------------------------------------------------------------------