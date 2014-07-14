--------------------------------------------------------------------------------
--- Second-level entity controller for the general handling of user entities.
--- Provides CRUD functionality for the use in top-level application component
--- controllers and covers business logic like input validation (if not done in
--- the view) and error handling (setting alerts, returning error pages).
---
--- In addition, this controller provides functionality to handle the favoriting
--- relationship between user entities and programs, since it is more likely to
--- create favoritings by adding programs as favorites as an user than vice
--- versa.
---
--- All controllers in this module take a pair consisting of a controller and a
--- possibly given alert for the case of a successful operation where the
--- controller usually is applicable on the result of this operation.
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module UsersController (
  doCreateUser,doUpdateUser,
  doAddFavoritingForCurrentUser,doRemoveFavoritingForCurrentUser
) where

import ProgramModel
import UserModel

import Alerts
import Authentication
import Controllers

--------------------------------------------------------------------------------
-- CRUD operations on user entities                                           --
--------------------------------------------------------------------------------

--- Returns a controller that persists a new user entity to the database if the
--- given username and email address are unique. Otherwise, an appropriate alert
--- can be set and a corresponding controller will be called.
--- @param nameErrData  - next controller and possibly given alert for the case
---   that the given username is not unique
--- @param emailErrData - next controller and possibly given alert for the case
---   that the given email address is not unique
--- @param successData  - next controller and possibly given alert for the case
---   of an successful user creation
--- @param creationData - required data for the user creation (name, email
---   address, password and repeated password)
doCreateUser
  :: ((String,String,String,String) -> Controller,Maybe Alert)
  -> ((String,String,String,String) -> Controller,Maybe Alert)
  -> (User                          -> Controller,Maybe Alert)
  -> (String,String,String,String)
  -> Controller
doCreateUser (nameNotUnqiueErrCtrl ,mNameNotUniqueErrAlert ) 
             (emailNotUniqueErrCtrl,mEmailNotUniqueErrAlert)
             (successCtrl          ,mSuccessAlert          )
             creationData@(name,email,password,_) =
  do mUserByName <- getUserByName name
     maybe (do mUserByEmail <- getUserByEmail email
               maybe (do hash     <- getUserHash name password
                         transRes <- UserModel.createUser (name,email,hash)
                         either (\user -> do maybeSetAlert mSuccessAlert
                                             successCtrl user)
                                (showTransactionErrorPage userCreationFailedErr)
                                transRes)
                     (\_ -> do maybeSetAlert mEmailNotUniqueErrAlert
                               emailNotUniqueErrCtrl creationData)
                     mUserByEmail)
           (\_ -> do maybeSetAlert mNameNotUniqueErrAlert
                     nameNotUnqiueErrCtrl creationData)
           mUserByName
  where
    userCreationFailedErr =
      "The user creation failed due to an unexpected internal error. See the "++
      "internal error message for additional details."

--- Returns a controller that updates an existing user entity in the database if
--- no uniqueness error occurs. Otherwise, an appropriate alert can be set and
--- a corresponding controller will be called.
--- @param nameErrData  - next controller and possibly given alert for the case
----  that the given username is not unique
--- @param emailErrData - next controller and possibly given alert for the case
---   that the given email address is not unique
--- @param successData  - next controller and possibly given alert for the case
---   of an successful user editing
--- @param user         - the edited user entity
doUpdateUser
  :: (User -> Controller,Maybe Alert)
  -> (User -> Controller,Maybe Alert)
  -> (User -> Controller,Maybe Alert)
  -> User
  -> Controller
doUpdateUser (nameNotUnqiueErrCtrl ,mNameNotUniqueErrAlert ) 
             (emailNotUniqueErrCtrl,mEmailNotUniqueErrAlert)
             (successCtrl          ,mSuccessAlert          )
             user =
  getStoredUser user >>=
  either (\olduser ->
    do nameok <- userNameOK olduser
       if nameok
        then do emailok <- userEmailOK olduser
                if emailok
                 then UserModel.updateUser user >>=
                      either (\_ -> do maybeSetAlert mSuccessAlert
                                       successCtrl user)
                             (showTransactionErrorPage userEditingFailedErr)
                 else do maybeSetAlert mEmailNotUniqueErrAlert
                         emailNotUniqueErrCtrl user
        else do maybeSetAlert mNameNotUniqueErrAlert
                nameNotUnqiueErrCtrl user)
    (showTransactionErrorPage userEditingFailedErr)
 where
  userNameOK olduser = if userName olduser == userName user
                       then return True
                       else getUserByName (userName user) >>=
                            maybe (return True) (\_ -> return False)
  userEmailOK olduser = if userEmail olduser == userEmail user
                        then return True
                        else getUserByEmail (userEmail user) >>=
                             maybe (return True) (\_ -> return False)
  userEditingFailedErr =
    "The user update failed due to an unexpected internal error. See the in"++
    "ternal error message for additional details."

--------------------------------------------------------------------------------
-- Adding and removing favoritings                                            --
--------------------------------------------------------------------------------

--- Adds a favoriting association for the currently authenticated user and a
--- given program. If the current user is not authenticated or does not exist,
--- an appropriate alert can be set and a corresponding controller will be
--- called.
--- @param noCurrUserFoundData - next controller and possibly given alert for
---   the case that the current user is not signed in or does not exist
--- @param successData         - next controller and possibly given alert for
---   the case of an successful adding of the association
--- @param prog                - the program to be added to the current users
---   favorites
doAddFavoritingForCurrentUser
  :: (Program -> Controller,Maybe Alert)
  -> (Controller           ,Maybe Alert)
  -> Program
  -> Controller
doAddFavoritingForCurrentUser (noCurrUserFoundCtrl,mNoCurrUserFoundErrAlert)
                              (successCtrl        ,mSuccessAlert          )
                              prog =
  do mUser <- getCurrentUser
     maybe (do maybeSetAlert mNoCurrUserFoundErrAlert
               noCurrUserFoundCtrl prog)
           (\user -> do transRes <- UserModel.addFavoriting (user,prog)
                        either (\_ -> do maybeSetAlert mSuccessAlert
                                         successCtrl)
                               (showTransactionErrorPage favAddingFailedErr)
                               transRes)
           mUser
  where
    favAddingFailedErr =
      "The adding of this favoriting association failed due to an unexpected "++
      "internal error. See the internal error message for additional details."

--- Removes a favoriting association for the currently authenticated user and a
--- given program. If the current user is not authenticated or does not exist,
--- an appropriate alert can be set and a corresponding controller will be
--- called.
--- @param noCurrUserFoundData - next controller and possibly given alert for
---   the case that the current user is not signed in or does not exist
--- @param successData         - next controller and possibly given alert for
---   the case of an successful adding of the association
--- @param prog                - the program to be removed from the current
---   users favorites
doRemoveFavoritingForCurrentUser
  :: (Program -> Controller,Maybe Alert)
  -> (Controller           ,Maybe Alert)
  -> Program
  -> Controller
doRemoveFavoritingForCurrentUser (noCurrUserFoundCtrl,mNoCurrUserFoundErrAlert)
                                 (successCtrl        ,mSuccessAlert           )
                                 prog =
  do mUser <- getCurrentUser
     maybe (do maybeSetAlert mNoCurrUserFoundErrAlert
               noCurrUserFoundCtrl prog)
           (\user -> do transRes <- UserModel.removeFavoriting (user,prog)
                        either (\_ -> do maybeSetAlert mSuccessAlert
                                         successCtrl)
                               (showTransactionErrorPage favRemovingFailedErr)
                               transRes)
           mUser
  where
    favRemovingFailedErr =
      "The removing of this favoriting association failed due to an unexpecte"++
      "d internal error. See the internal error message for additional details."

--------------------------------------------------------------------------------