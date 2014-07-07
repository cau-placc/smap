--------------------------------------------------------------------------------
--- This module provides alerts and operations on alerts like storing alerts in
--- sessions, getting active alerts from the current session data and rendering 
--- alerts in views (alerts are comparable with Spicey's default page messages).
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module Alerts (
  Alert,AlertType(..),
  setAlert,getAlert,
  maybeSetAlert,
  renderAlert
) where

import Bootstrap3
import Global
import Html5

import Session

--------------------------------------------------------------------------------
-- General definition                                                         --
--------------------------------------------------------------------------------

--- Alerts can be stored in a temporary session store to show up on the next
--- displayed view. Every alert contains
--- - an alert type (for rendering purposes)
--- - an alert message (may contain HTML markup).
--- The definition of alerts is independent from their rendering which is 
--- specified in the function `renderAlert`.
type Alert = (AlertType,String)

--- Currently available alert types.
--- @cons ErrorAlert   - type for error alerts
--- @cons InfoAlert    - type for info alerts  (no styling support yet)
--- @cons SuccessAlert - type for success alerts
--- @cons WarningAlert - type for warning alerts (no styling support yet)
data AlertType = ErrorAlert | InfoAlert | SuccessAlert | WarningAlert

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
setAlert nAlert = putSessionData nAlert alert

--- Gets the current alert and deletes it from the session.
getAlert :: IO (Maybe Alert)
getAlert = 
  do mAlert <- getSessionData alert
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
renderAlert :: Alert -> HtmlExp
renderAlert (alertType,alertMsg) =
  stdModal modalId labelId
    [icon,text $ ' ':header]
    [text alertMsg]
    [buttonButton [class "btn btn-default",modalDismiss]
      [glyphicon "remove",text " Dismiss"]]
  where 
    modalId = "alert-"++alertTypeStr++"-dialog-box"
    labelId = "alert-dialog-box-title"
    (alertTypeStr,header,icon) = 
      case alertType of
        ErrorAlert -> ("error"  
                      ,"Oops, an error occured!"
                      ,glyphicon "exclamation-sign")
        _          -> ("success"
                      ,"Success!"
                      ,glyphicon "ok-sign")

--------------------------------------------------------------------------------