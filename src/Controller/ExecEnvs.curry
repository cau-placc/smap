--------------------------------------------------------------------------------
--- Second-level entity controller for the general handling of execution
--- environments (see module `Model.ExecEnv`), languages and systems. Provides
--- CRUD functionality for the use in top-level application component
--- controllers and covers business logic like input validation (if not done in
--- the view) and error handling (settings alerts, returning error pages).
---
--- All controllers in this module take a pair consisting of a controller and a
--- possibly given alert for the case of a successful operation where the
--- controller usually is applicable on the result of this operation.
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module Controller.ExecEnvs (
  doCreateLanguage,doCreateSystem
) where

import Model.ExecEnv

import System.Alerts
import System.Controllers

--------------------------------------------------------------------------------
-- CRUD operations on execution environments, languages and systems           --
--------------------------------------------------------------------------------

--- ...
doCreateLanguage
  :: (Language -> Controller,Maybe Alert)
  -> (String,String,String)
  -> Controller
doCreateLanguage (successCtrl,mSuccessAlert) creationData =
  do transRes <- Model.ExecEnv.createLanguage creationData
     either (\lang -> do maybeSetAlert mSuccessAlert
                         successCtrl lang)
            (showTransactionErrorPage languageCreationFailedErr)
            transRes
  where
    languageCreationFailedErr =
      "The language creation failed due to an unexpected internal error. See "++
      "the internal error message for additional details."

--- ...
doCreateSystem
  :: (System -> Controller,Maybe Alert)
  -> (String,String,Language)
  -> Controller
doCreateSystem (successCtrl,mSuccessAlert) creationData =
  do transRes <- Model.ExecEnv.createSystem creationData
     either (\system -> do maybeSetAlert mSuccessAlert
                           successCtrl system)
            (showTransactionErrorPage languageCreationFailedErr)
            transRes
  where
    languageCreationFailedErr =
      "The system creation failed due to an unexpected internal error. See th"++
      "e internal error message for additional details."

--------------------------------------------------------------------------------