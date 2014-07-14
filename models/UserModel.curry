--------------------------------------------------------------------------------
--- This module provides an interface to the user model. Thus, it provides
--- functionality for accessing and modifying user attributes, basic database
--- operations for storing, reading, updating and deleting user entities (CRUD
--- operations, transactions, queries) with respect to database consistency and
--- predicates on users. In addition, this module provides operations for adding
--- and removing favoriting associations between users and programs.
---
--- Further business logic concerning users is stored in the corresponding
--- second-level controller module `UsersController`.
---
--- (Note: The actual definition of the `User` datatype (and its accessors) can
--- be found in the module `Smap` which should not be modified or imported for
--- stackability reasons. Always modify or import `UserModel` instead.)
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module UserModel (
  User,UserKey,setUserName,userName,setUserEmail,userEmail,setUserHash,userHash,
  setUserIsAdmin,userIsAdmin,

  createUser,updateUser,getStoredUser,
  getUserByName,getUserByNameWith,getUserByEmail,getUserByEmailWith,
  addFavoriting,removeFavoriting,
  authored,favorites
) where

import KeyDatabase
import Maybe

import ProgramModel
import Smap

--------------------------------------------------------------------------------
-- Database CRUD operations on user entities                               --
--------------------------------------------------------------------------------

-- Creating, updating and deleting users

--- Creates a new user entity in the database.
--- @param name  - the name of the user
--- @param email - the email address of the user
--- @param hash  - the generated hash string
createUser :: (String,String,String) -> IO (Either User TError)
createUser (name,email,hash) = runT $ newUser name email hash Nothing

--- Updates an existing user entity in the database.
--- @param user - the User object to be updated
updateUser :: User -> IO (Either () TError)
updateUser = runT . Smap.updateUser

-- Reading user entities from the database

--- Gets the user entity currently stored corresponding to a given user
--- (which might be modified). `Nothing` is returned if no such user exists.
--- @param user - the User object whose stored version should be retrieved
getStoredUser :: User -> IO (Either User TError)
getStoredUser user = runT (getUser (userKey user))

--- Gets the user entity with the given name from the database. `Nothing` is
--- returned (as an I/O action) if no such user exists.
--- @param name - the name of the user
getUserByName :: String -> IO (Maybe User)
getUserByName name = getUserByNameWith name (Nothing,Nothing)

--- Gets the user entity with the given name and specific attribute values from
--- the database. `Nothing` is returned (as an I/O action) if no such user
--- exists.
--- @param name  - the name of the user
--- @param attrs - possibly given email address and user hash     
getUserByNameWith :: String -> (Maybe String,Maybe String) -> IO (Maybe User)
getUserByNameWith name (mEmail,mHash) =
  do users <- getAllUsersWith (Just name,mEmail,mHash)
     return $ listToMaybe users

--- Gets the user entity with the given email address from the database.
--- `Nothing` is returned (as an I/O action) if no such user exists.
--- @param email - the email address of the user
getUserByEmail :: String -> IO (Maybe User)
getUserByEmail email = getUserByEmailWith email (Nothing,Nothing)

--- Gets a user entity with the given email address and specific attribute
--- values from the database. `Nothing` is returned (as an I/O action) if no 
--- such user exists.
--- @param email - the email address of the user
--- @param attrs - possibly given email address and user hash 
getUserByEmailWith :: String -> (Maybe String,Maybe String) -> IO (Maybe User)
getUserByEmailWith email (mName,mHash) =
  do users <- getAllUsersWith (mName,Just email,mHash)
     return $ listToMaybe users

-- Returns all currently persisted user entities with specific attributes.
-- @param attrs - possibly given name, email address and user hash
getAllUsersWith :: (Maybe String,Maybe String,Maybe String) -> IO [User]
getAllUsersWith (mName,mEmail,mHash) = runQ $ queryCondUser cond
  where 
    cond user =
      maybe True (\name  -> userName  user == name ) mName  &&
      maybe True (\email -> userEmail user == email) mEmail &&
      maybe True (\hash  -> userHash  user == hash ) mHash

--------------------------------------------------------------------------------
-- Adding and removing favoritings                                            --
--------------------------------------------------------------------------------

--- Creates a new favoriting relation between a given user and a given program.
--- @param user - the user who favorites the program
--- @param prog - the favorited program
addFavoriting :: (User,Program) -> IO (Either () TError)
addFavoriting (user,prog) =
  runT $ newFavoriting (userKey user) (programKey prog) 

--- Deletes an existing favoriting relation between a user and a program.
--- @param user - the user from the existing relation
--- @param prog - the program from the existing relation
removeFavoriting :: (User,Program) -> IO (Either () TError)
removeFavoriting (user,prog) =
  runT $ Smap.deleteFavoriting (userKey user) (programKey prog)

--------------------------------------------------------------------------------
-- Predicates on users and user attributes                                    --
--------------------------------------------------------------------------------

--- Returns `True` if the user with the given name is the author of the given
--- program.
--- @param name - the name of the user
--- @param prog - the program
authored :: String -> Program -> Bool
authored name = (name==) . userName . programAuthor

--- Returns `True` if the user with the given name favorites the given program.
--- @param name - the name of the user
--- @param prog - the program
favorites :: String -> Program -> Bool
favorites name = (name `elem`) . map userName . programFavoriters

--------------------------------------------------------------------------------