--------------------------------------------------------------------------------
--- This module defines access types for the core components of this application
--- and tests the authorization of related operations. Authorization is
--- primarily based on the type of the interacting user which is encapsulated
--- in the data type `AuthZData` (see module `Authorization`). The computed
--- access results can be used to authorize controllers or define authorized
--- elements on views.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2014
--------------------------------------------------------------------------------

module AuthorizedOperations (
  SmapIEAccessType(..),BrowserAccessType(..),AuthNAccessType(..),
  AdminAccessType(..),
  smapIEOperation,browserOperation,authNOperation,adminOperation,isAdmin
) where

import Authorization
import ProgramModel
import UserModel

--------------------------------------------------------------------------------
-- SmapIE access types and operations                                         --
--------------------------------------------------------------------------------

--- Access types for SmapIE operations. A SmapIE operation is one of the
--- following.
--- @cons ShowSmapIE         - generally shows the interactive editor
--- @cons CreateProgram      - save a new program to Smap
--- @cons CreateVersion prog - save a new version for `prog` to Smap
--- @cons ExecuteProgram     - executes the current source code
data SmapIEAccessType
  = ShowSmapIE (Maybe Program) | CreateProgram | CreateVersion Program
  | ExecuteProgram

--- Checks the authorization of SmapIE operations.
--- @param accessType - the SmapIE access type
--- @param authzData  - the current authorization data
smapIEOperation :: SmapIEAccessType -> AuthZData -> AccessResult
smapIEOperation accessType authzData =
  case accessType of
    ShowSmapIE mProg -> case mProg of
      (Just prog) -> case authzData of
        Guest         -> if programIsVisible prog
                         then AccessGranted
                         else AccessDenied openProgDeniedErr
        Standard name -> if programIsVisible prog || name `authored` prog
                         then AccessGranted
                         else AccessDenied openProgDeniedErr
        Admin    _    -> AccessGranted
      Nothing     -> AccessGranted
    CreateProgram -> case authzData of
      Guest -> AccessDenied createProgDeniedErr
      _     -> AccessGranted
    CreateVersion prog -> case authzData of
      Guest         -> AccessDenied createVersDeniedGErr
      Standard name -> if name `authored` prog
                       then AccessGranted
                       else AccessDenied createVersDeniedSErr
      Admin    _    -> AccessGranted
      -- Admin    name -> if name `authored` prog
      --                  then AccessGranted
      --                  else AccessDenied createVersDeniedAErr
    _ -> AccessGranted
  where
    openProgDeniedErr =
      "You are not allowed to access this program."
    createProgDeniedErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    createVersDeniedGErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    createVersDeniedSErr =
      "You are not allwowed to add a new version to this program."

--------------------------------------------------------------------------------
-- Browser access types and operations                                        --
--------------------------------------------------------------------------------

--- Access types for browser operations. A browser Operation is one of the
--- following.
--- @cons ShowAllPrograms          - shows all programs on Smap
--- @cons ShowUserPrograms         - shows all programs of the current user
--- @cons ShowUserFavorites        - shows all favorites of the current user
--- @cons Show Program prog        - shows `prog` in the browser
--- @cons AddToFavorites prog      - adds `prog` to the current users favorites
--- @cons RemoveFromFavorites prog - removes `prog` from the users favorites
--- @cons ModifyProgram prog       - modify description of `prog`
--- @cons DeleteProgram prog       - deletes `prog` from Smap
--- @cons ShowAllTags              - shows all tags on Smap
--- @cons DeleteTag tag            - delete a tag
data BrowserAccessType 
  = ShowAllPrograms | ShowUserPrograms | ShowUserFavorites | ShowProgram Program
  | MakeVisible Program | AddToFavorites Program | RemoveFromFavorites Program 
  | ModifyProgram Program | DeleteProgram Program | CreateComment
  | ShowAllTags | DeleteTag Tag

--- Checks the authorization of browser operations
--- @param accessType - the browser access type
--- @param authzData  - the current authorization data
browserOperation :: BrowserAccessType -> AuthZData -> AccessResult
browserOperation accessType authzData =
  case accessType of
    ShowUserPrograms -> case authzData of
      Guest -> AccessDenied showUserProgsDeniedErr
      _     -> AccessGranted
    ShowUserFavorites -> case authzData of
      Guest -> AccessDenied showUserFavsDeniedErr
      _     -> AccessGranted
    ShowProgram prog -> case authzData of
      Guest         -> if programIsVisible prog
                       then AccessGranted
                       else AccessDenied showProgDeniedErr
      Standard name -> if programIsVisible prog || name `authored` prog
                       then AccessGranted
                       else AccessDenied showProgDeniedErr
      Admin    _    -> AccessGranted
    MakeVisible prog -> case authzData of
      Guest         -> AccessDenied makeVisibleDeniedErr
      Standard name -> if (not $ programIsVisible prog) && name `authored` prog
                       then AccessGranted
                       else AccessDenied makeVisibleDeniedErr
      Admin _       -> if not (programIsVisible prog)
                       then AccessGranted
                       else AccessDenied makeVisibleDeniedErr
    AddToFavorites prog -> case authzData of
      Guest         -> AccessDenied addToFavsDeniedGErr
      Standard name -> if name `favorites` prog || name `authored` prog
                       then AccessDenied addToFavsDeniedSErr
                       else AccessGranted
      Admin    name -> if name `favorites` prog || name `authored` prog
                       then AccessDenied addToFavsDeniedAErr
                       else AccessGranted
    RemoveFromFavorites prog -> case authzData of
      Guest         -> AccessDenied remFromFavsDeniedGErr
      Standard name -> if name `favorites` prog
                       then AccessGranted
                       else AccessDenied remFromFavsDeniedSErr
      Admin    name -> if name `favorites` prog
                       then AccessGranted
                       else AccessDenied remFromFavsDeniedAErr
    ModifyProgram prog -> case authzData of
      Guest         -> AccessDenied modProgDeniedGErr
      Standard name -> if name `authored` prog
                       then AccessGranted
                       else AccessDenied modProgDeniedSErr
      Admin    _    -> AccessGranted 
    DeleteProgram prog -> case authzData of
      Guest         -> AccessDenied delProgDeniedGErr
      Standard name -> if name `authored` prog
                       then AccessGranted
                       else AccessDenied delProgDeniedSErr
      Admin    _    -> AccessGranted 
    CreateComment -> case authzData of
      Guest -> AccessDenied createCommentDeniedErr
      _     -> AccessGranted 
    DeleteTag _ -> case authzData of
      Guest      -> AccessDenied delTagDeniedErr
      Standard _ -> AccessDenied delTagDeniedErr
      Admin    _ -> AccessGranted 
    _ -> AccessGranted
  where
    showUserProgsDeniedErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    showUserFavsDeniedErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    showProgDeniedErr =
      "You are not allowed to access this program."
    makeVisibleDeniedErr =
      "You have not the rights to make this program available for other users"++
      ". Only its author is allowed to do that."
    addToFavsDeniedGErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    addToFavsDeniedSErr =
      "You are either the author of this program or you already favorited it "++
      "(you are not allowed to add your own programs to your favorites)."
    addToFavsDeniedAErr =
      "You are either the author of this program or you already favorited it "++
      "(you are not allowed to add your own programs to your favorites)."
    remFromFavsDeniedGErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    remFromFavsDeniedSErr =
      "You have not favorited this program."
    remFromFavsDeniedAErr =
      "You have not favorited this program."
    modProgDeniedGErr = 
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    modProgDeniedSErr =
      "You are not the author of this program. You are not allowed to modify "++
      "the programs of other users."
    delProgDeniedGErr = 
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    delProgDeniedSErr =
      "You are not the author of this program. You are not allowed to delete "++
      "the programs of other users."
    delTagDeniedErr = 
      "You are not allowed to delete a tag."
    createCommentDeniedErr =
      "You need to <a href=\"?signin\">sign in</a> to write comments."

--------------------------------------------------------------------------------
-- Authentication related access types and operations                         --
--------------------------------------------------------------------------------

--- Access types for authentication operations. A authentication Operation is
--- one of the following.
--- @cons SignUp          - creates a new user in the database
--- @cons SignIn          - authenticates the current user
--- @cons SignOut         - signs out the currently authenticated user
--- @cons SendNewPassword - sends a new password to the users email address
--- @cons ChangePasswd    - change password of user which is signed in
data AuthNAccessType = SignUp | SignIn | SignOut | SendNewPassword
                     | ChangePasswd

--- Checks the authorization of authentication operations.
--- @param accessType - the authentication access type
--- @param authzData  - the current authorization data
authNOperation :: AuthNAccessType -> AuthZData -> AccessResult
authNOperation accessType authzData =
  case accessType of
    SignUp -> case authzData of
      Guest -> AccessGranted
      _     -> AccessDenied signUpDeniedErr
    SignIn -> case authzData of
      Guest -> AccessGranted
      _     -> AccessDenied signInDeniedErr
    SignOut -> case authzData of
      Guest -> AccessDenied signOutDeniedErr
      _     -> AccessGranted
    ChangePasswd -> case authzData of
      Guest -> AccessDenied modUserDeniedErr
      _     -> AccessGranted
    _ -> AccessGranted
  where
    signUpDeniedErr =
      "You are already signed in. <a href=\"?signout\">Sign out</a> to proceed." 
    signInDeniedErr =
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."
    signOutDeniedErr =
      "You are already signed in. <a href=\"?signout\">Sign out</a> to pe"++
          "rform this action."
    modUserDeniedErr = 
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."

--------------------------------------------------------------------------------
-- Administrational operations                                                --
--------------------------------------------------------------------------------

--- Access types for administrational operations. A administrational operation
--- is one of the following.
--- @cons CreateLanguage - creates a new language entity 
--- @cons CreateSystem   - creates a new system entity
data AdminAccessType = CreateLanguage | CreateSystem

--- Checks the authorization of administrational operations.
--- @param authzData - the current authorization data
adminOperation :: AdminAccessType -> AuthZData -> AccessResult
adminOperation _ authzData =
  case authzData of
    Admin _ -> AccessGranted
    _       -> AccessDenied adminOpDeniedErr
  where
    adminOpDeniedErr =
      "You have not the rights to proceed."

--- Checks whether current user is administrator.
--- @param authzData - the current authorization data
isAdmin :: AuthZData -> Bool
isAdmin authzData = case authzData of
    Admin _ -> True
    _       -> False

--------------------------------------------------------------------------------