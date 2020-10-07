--------------------------------------------------------------------------------
--- This module provides alerts and operations on alerts like storing alerts in
--- sessions, getting active alerts from the current session data and rendering 
--- alerts in views (alerts are comparable with Spicey's default page messages).
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2020
--------------------------------------------------------------------------------

module System.Alerts (
  Alert(..),
  setAlert,getAlert,
  maybeSetAlert,
  renderAlert
) where

import Global

import HTML.Base ( fromFormReader )
import HTML.Bootstrap3
import HTML.Html5
import HTML.Session

import System.SmapHtml

--------------------------------------------------------------------------------
-- General definition                                                         --
--------------------------------------------------------------------------------

--- Alerts can be stored in a temporary session store to show up on the next
--- displayed view. Every alert contains
--- - an alert type (for rendering purposes)
--- - an alert message (may contain HTML markup).
--- The definition of alerts is independent from their rendering which is 
--- specified in the function `renderAlert`.
--- Currently available alert types:
--- @cons ErrorAlert   - type for error alerts
--- @cons InfoAlert    - type for info alerts  (no styling support yet)
--- @cons SuccessAlert - type for success alerts
--- @cons WarningAlert - type for warning alerts (no styling support yet)
data Alert = ErrorAlert String
           | InfoAlert String
           | SuccessAlert String
           | WarningAlert String

--------------------------------------------------------------------------------
-- Storing alerts in sessions                                                 --
--------------------------------------------------------------------------------

-- Basic storing

-- Definition of the session state to store the alert.
alert :: Global (SessionStore Alert)
alert = global emptySessionStore Temporary

--- Sets the alert of the current session.
--- @param nAlert - the next alert
setAlert :: Alert -> IO ()
setAlert nAlert = writeSessionData alert nAlert

--- Gets the current alert and deletes it from the session.
getAlert :: IO (Maybe Alert)
getAlert = 
  do mAlert <- fromFormReader $ getSessionMaybeData alert
     removeSessionData alert
     return mAlert

-- Operations for special cases

--- Sets the alert of the current session if one is given. Does nothing if
--- applied on `Nothing`.
--- @param mAlert - a possibly given alert
maybeSetAlert :: Maybe Alert -> IO ()
maybeSetAlert = maybe done setAlert

--------------------------------------------------------------------------------
-- Rendering alerts                                                           --
--------------------------------------------------------------------------------

--- The standard rendering for alerts. Currently, alerts are rendered as
--- modified Bootstrap 3 modals (http://getbootstrap.com/javascript/#modals)
--- with additional contextual classes (representing the alert types) whose
--- styles are specified in `public/css/smap.css`.
--- @param alert - the current alert
renderAlert :: HTML h => Alert -> h
renderAlert alert =
  stdModal modalId labelId
    [icon,text $ ' ':header]
    [text alertMsg]
    [buttonButton [classA "btn btn-default",modalDismiss]
      [glyphicon "remove",text " Dismiss"]]
  where 
    modalId = "alert-" ++ alertTypeStr ++ "-dialog-box"
    labelId = "alert-dialog-box-title"
    (alertTypeStr,alertMsg,header,icon) = 
      case alert of
        ErrorAlert   s -> ("error",s,"Oops, an error occured!",execErrorIcon)
        InfoAlert    s -> ("success",s,"For your information:",infoIcon)
        SuccessAlert s -> ("success",s,"Success!",execSuccessIcon)
        WarningAlert s -> ("success",s,"Warning:",execSuccessIcon)

--------------------------------------------------------------------------------