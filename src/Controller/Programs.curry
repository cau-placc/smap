--------------------------------------------------------------------------------
--- Second-level entity controller for the general handling of programs.
--- Provides CRUD functionality for the use in top-level application component
--- controllers and covers business logic like input validation (if not done in
--- the view) and error handling (settings alerts, returning error pages).
---
--- All controllers in this module take a pair consisting of a controller and a
--- possibly given alert for the case of a successful operation where the
--- controller usually is applicable on the result of this operation.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version October 2022
--------------------------------------------------------------------------------

module Controller.Programs (
  doCreateProgram,doCreateProgramVersion,
  doUpdateProgramMetadata,doUpdateProgramMetadataWithTags,
  doDeleteProgram
) where

import Data.Char ( toLower)
import Data.Time ( getLocalTime )

import Model.Program

import System.Alerts
import System.Controllers

--------------------------------------------------------------------------------
-- CRUD operations on programs                                                --
--------------------------------------------------------------------------------

--- Returns a controller that persists a new program to the database.
--- @param successData  - next controller and possibly given alert for the case
---   of a successful program creation
--- @param creationData - required data for the program creation (title,
---   description, visibility state, language, author, code and a string
---   containing the associated tags as a list of space-separated names)
doCreateProgram
  :: (Program -> Controller,Maybe Alert)
  -> (String,String,Bool,Language,User,String,String)
  -> Controller
doCreateProgram (successCtrl,mSuccessAlert)
                (title,descr,vis,lang,user,code,tagNamesStr) =
  do date    <- getLocalTime
     tResult <- Model.Program.createProgram (title,descr,vis,lang,user
                                           ,code,date,tagNames)
     either (\prog -> do maybeSetAlert mSuccessAlert
                         successCtrl prog)
            (showTransactionErrorPage programCreationFailedErr)
            tResult
  where
    tagNames = map (map toLower) (words tagNamesStr)

    programCreationFailedErr =
      "The program creation failed due to an unexpected internal error. See t"++
      "he internal error message for additional details."

--- Returns a controller that persists a new version to a given program.
--- @param successData  - next controller and possibly given alert for the case
---   of a successful version creation
--- @param creationData - required data for the version creation (version
---   number, code, version message and related program)
doCreateProgramVersion
  :: (Version -> Controller,Maybe Alert)
  -> (Int,String,String,Program)
  -> Controller
doCreateProgramVersion (successCtrl,mSuccessAlert)
                       (number,code,message,prog) =
  do date     <- getLocalTime
     transRes <- createProgramVersion (number,code,message,date,prog)
     either (\vers -> do maybeSetAlert mSuccessAlert
                         successCtrl vers)
            (showTransactionErrorPage programVersionCreationFailedErr)
            transRes
  where
    programVersionCreationFailedErr =
      "The creation of the new version to this program failed due to an unexp"++
      "ected internal error. See the internal error message for additional de"++
      "tails."

--- Returns a controller that updates an existing program and proceeds with the
--- given controller if the operation succeeds. Otherwise the transaction error
--- page will be displayed.
--- @param successData  - next controller and possibly given alert for the case
---   of an successful program editing
--- @param prog         - the updated program
doUpdateProgramMetadata
  :: (Program -> Controller,Maybe Alert) -> Program -> Controller
doUpdateProgramMetadata (successCtrl,mSuccessAlert) prog =
  do transRes <- Model.Program.updateProgramMetadata prog
     either (\_ -> do maybeSetAlert mSuccessAlert
                      successCtrl prog)
            (showTransactionErrorPage programEditingFailedErr)
            transRes

programEditingFailedErr :: String
programEditingFailedErr =
  "The program update failed due to an unexpected internal error. See the "++
  "internal error message for additional details."

--- Returns a controller that updates an existing program and proceeds with the
--- given controller if the operation succeeds. Otherwise the transaction error
--- page will be displayed.
--- @param successData  - next controller and possibly given alert for the case
---   of an successful program editing
--- @param tagstring    - the string containing the updated tags
--- @param prog         - the updated program
doUpdateProgramMetadataWithTags
  :: (Program -> Controller,Maybe Alert) -> [String] -> Program -> Controller
doUpdateProgramMetadataWithTags (successCtrl,mSuccessAlert) tagstring prog =
  do transRes <- Model.Program.updateProgramMetadataWithTags tagstring prog
     either (\_ -> do maybeSetAlert mSuccessAlert
                      successCtrl prog)
            (showTransactionErrorPage programEditingFailedErr)
            transRes

--- Returns a controller that deletes a given program. This includes the
--- deletion of the associated metadata entity, all associated versions, all
--- associated taggings, all associated comments and all associated favoritings.
--- @param successData - next controller and possibly given alert for the case
---   of an successful deletion
--- @param prog        - the program to be deleted
doDeleteProgram
  :: (Controller,Maybe Alert)
  -> Program
  -> Controller
doDeleteProgram (successCtrl,mSuccessAlert) prog =
  do transRes <- Model.Program.deleteProgram prog
     either (\_ -> do maybeSetAlert mSuccessAlert
                      successCtrl)
            (showTransactionErrorPage programDeletionFailedErr)
            transRes
  where
    programDeletionFailedErr =
      "The program deletion failed due to an unexpected internal error. See t"++
      "he internal error message for additional details."

--------------------------------------------------------------------------------