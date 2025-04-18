--------------------------------------------------------------------------------
--- This library contains operations to support the management of user
--- authentication. It provides operations for password generation, password
--- hashing (based on hashing algorithms from Unix), and storing authentication
--- data in sessions.
---
--- @author Michael Hanus, Lasse Kristopher Meyer
--- @version October 2022
--------------------------------------------------------------------------------

module System.Authentication (
  AuthNData,getSessionAuthNData,signInToSession,signOutFromSession,
  getCurrentUser,
  getUserHash,randomPassword
) where

import System.FilePath ( (</>) )

import HTML.Base
import HTML.Session

import Model.User
import Crypto.Hash

--------------------------------------------------------------------------------
-- Authentication data storing                                                --
--------------------------------------------------------------------------------

--- The type synonym for authentication data in this application. Authentication
--- data consists of
--- - the username of the currently logged in user
--- - the admin state of the currently loggied in user.
type AuthNData = (String,Bool)

-- Definition of the session state to store the authentication data.
sessionAuthNData :: SessionStore AuthNData
sessionAuthNData = sessionStore "sessionAuthData"

--- Gets the current authentication data from the session store.
getSessionAuthNData :: IO (Maybe AuthNData)
getSessionAuthNData = fromFormReader $ getSessionMaybeData sessionAuthNData

--- Stores new authentication data in the current session (after the user
--- authentication).
--- @param authNData - authentication data of the user
signInToSession :: AuthNData -> IO ()
signInToSession authNData = putSessionData sessionAuthNData authNData

--- Removes authentication data from the current session.
signOutFromSession :: IO ()
signOutFromSession = removeSessionData sessionAuthNData

--------------------------------------------------------------------------------
-- Operations on current session data                                         --
--------------------------------------------------------------------------------

--- Returns the user entity associated to the name in the current session data.
--- `Nothing` is returned if the session data is empty or no user with the
--- corresponding name exists.
getCurrentUser :: IO (Maybe User)
getCurrentUser =
  do mAuthNData <- getSessionAuthNData
     maybe (return Nothing)
           (\(name,_) -> getUserByName name)
           mAuthNData

--------------------------------------------------------------------------------
-- Operations for hashing                                                     --
--------------------------------------------------------------------------------

--- Gets a hash string for a user name and password. The generated hash string
--- should be stored for the user instead of the password.
--- @param username - the username of the user
--- @param password - the password of the user in plaintext
getUserHash :: String -> String -> IO String
getUserHash username userpassword = do
  let systemkey = "smap2014" -- change this key for every spicey instance
  getHash $ username ++ userpassword ++ systemkey

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
randomPassword :: Int -> IO String
randomPassword = randomString

--------------------------------------------------------------------------------