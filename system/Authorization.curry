--------------------------------------------------------------------------------
--- This module specifies the access authorization to web pages and operations.
--- The actual result to an authorization request is computed in the module
--- `AuthorizedOperations`.
--- 
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module Authorization (
  AccessResult(..),AuthZData(..),
  checkAuthorization,
  getAuthZData,byAuthorization,getUsernameFromAuthZData
) where

import Authentication
import Controllers

--------------------------------------------------------------------------------
-- Requesting accesses and access results                                     --
--------------------------------------------------------------------------------

--- The result of checking an authorization. The access is either granted or
--- denied with a string explaining the reason.
--- @cons AccessGranted       - result for granted access requests
--- @cons AccessDenied reason - result for denied access requests (with reason)
data AccessResult = AccessGranted | AccessDenied String

--- Checks the results of an authorization access. If the access is granted, we
--- proceed with the given controller and apply it on the authorization data,
--- otherwise we display an error page.
--- @param getAccResult - gets the access result from authorization data
--- @param controller   - the controller to be executed when access is granted
checkAuthorization
  :: (AuthZData -> AccessResult) -> (AuthZData -> Controller) -> Controller
checkAuthorization getAccResult controller = 
  do azData    <- getAuthZData
     accResult <- return $ getAccResult azData
     case accResult of
       AccessGranted       -> controller azData
       AccessDenied reason -> showAccessDeniedErrorPage reason

--------------------------------------------------------------------------------
-- Authorization data                                                         --
--------------------------------------------------------------------------------

--- The data type containing all information needed in access requests.
--- Currently, authorization data in this application consists of the user type
--- and name.
--- @cons Guest         - user type: guest
--- @cons Standard name - user type: standard user with name `name`
--- @cons Admin name    - user type: admin with name `name`
data AuthZData = Guest | Standard String | Admin String

--- Turns authentication data stored in the current session to authorization
--- data for access requests.
getAuthZData :: IO AuthZData
getAuthZData =
  do mAuthNData <- getSessionAuthNData
     return $ maybe Guest
                    (\(userName,userIsAdmin) -> 
                      if userIsAdmin 
                      then Admin    userName
                      else Standard userName)
                    mAuthNData

--- Returns a value of a specified type depending on an access result to an
--- authorization request.
--- @param accessResult  - the access result to an authorization request
--- @param grantedRetVal - the value for granted access
--- @param deniedRetVal  - the value for denied access (depending on the reason)
byAuthorization :: AccessResult -> a -> (String -> a) -> a
byAuthorization accessResult grantedRetVal deniedRetVal =
  case accessResult of
    AccessGranted       -> grantedRetVal
    AccessDenied reason -> deniedRetVal reason

--- Extracts the username from given authorization data. Returns `Nothing` if
--- the user is not logged in (and has role `Guest`).
--- @param azData - the current authorization data
getUsernameFromAuthZData :: AuthZData -> Maybe String
getUsernameFromAuthZData azData =
  case azData of
    Guest             -> Nothing
    Standard username -> Just username
    Admin    username -> Just username

--------------------------------------------------------------------------------