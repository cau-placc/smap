--------------------------------------------------------------------------------
--- This module provides controllers for operations related to the interactive
--- editor (SmapIE) such as displaying the basic views for the IE itself,
--- displaying forms for program and version creation, performing the actual
--- program and version creation and executing programs.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version November 2018
--------------------------------------------------------------------------------

module Controller.SmapIE (
  smapIEController
) where

import Maybe
import Time
import HTML.Base (urlencoded2string)

import Model.ExecEnv
import Model.Program
import Model.User

import Controller.Programs

import View.SmapIE

import System.Alerts
import System.Authorization
import System.AuthorizedOperations
import System.Controllers
import System.Execution
import System.Url

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main IE controller which delegates tasks to other controllers in this
--- module depending on the URL path.
--- @param url - the current URL
smapIEController :: Url -> Controller
smapIEController url@(path,params) =
  case path of
    ["new",langname] -> showBlankSmapIE langname
    ["upload"]       -> maybe (showInvalidUrlErrorPage url)
                          (\langname ->
                            maybe (showInvalidUrlErrorPage url)
                              (\prog -> showUploadSmapIE langname
                                          (urlencoded2string prog))
                              (lookup "program" params))
                          (lookup "lang" params)
    [progKey]           -> validateKeyAndApply
                             (readProgramKey progKey)
                             url showProgramInSmapIE
    _                   -> showInvalidUrlErrorPage url
 where
  
--------------------------------------------------------------------------------
-- SmapIE Controllers                                                         --
--------------------------------------------------------------------------------

-- Returns a controller that shows the blank SmapIE for creating a new program
-- with the given language. Returns an error page if no language with the given
-- name exists.
-- @param langName - the name of the language the IE instance is attached to
showBlankSmapIE :: String -> Controller
showBlankSmapIE langName =
  do mExecEnv <- getExecEnvByLanguageName langName
     maybe (showStdErrorPage langNotFoundErr)
           (\execEnv -> showSmapIE Nothing execEnv Nothing 
                                   (initCode execEnv)
                                   (initSystemKey execEnv))
           mExecEnv
  where 
    initCode (lang,_)         = languageTemplate $ lang
    initSystemKey (_,systems) = showSystemKey $ head $ systems
    langNotFoundErr =
      "Sorry, we could not find this language in our database. Choose <code>E"++
      "ditor</code> in the navigation bar to get a list of all available lang"++
      "uages."

-- Returns a controller that shows the SmapIE for the given language
-- with an initial program text (e.g., which is uploaded).
-- Returns an error page if no language with the given name exists.
-- @param langName - the name of the language the IE instance is attached to
-- @param initcode - initial code shown in the program widget
showUploadSmapIE :: String -> String -> Controller
showUploadSmapIE langName initcode =
  do mExecEnv <- getExecEnvByLanguageName langName
     maybe (showStdErrorPage langNotFoundErr)
           (\execEnv -> showSmapIE Nothing execEnv Nothing initcode
                                   (initSystemKey execEnv))
           mExecEnv
  where 
    initCode (lang,_)         = languageTemplate $ lang
    initSystemKey (_,systems) = showSystemKey $ head $ systems
    langNotFoundErr =
      "Sorry, we could not find this language in our database. Choose <code>E"++
      "ditor</code> in the navigation bar to get a list of all available lang"++
      "uages."

-- Returns a controller that opens a program with a given key in the IE if such
-- a program exists. Otherwise, the error page will be displayed.
-- @param progKey - the key of the program that will be opened
showProgramInSmapIE :: ProgramKey -> Controller
showProgramInSmapIE progKey = 
  do mProg <- getProgramByKey progKey
     maybe (showStdErrorPage progNotFoundErr)
           (\prog -> do mExecEnv <- getExecEnvByLanguageKey $ langKey prog
                        maybe (showStdErrorPage execEnvNotFoundErr)
                              (\execEnv -> showSmapIE (Just prog) execEnv
                                                      Nothing (initCode prog)
                                                      (initSystemKey execEnv))
                              mExecEnv)
           mProg
  where
    langKey                   = languageKey . programImplLang
    initCode prog             = versionSourceCode $ programLatestVersion prog
    initSystemKey (_,systems) = showSystemKey $ head $ systems
    progNotFoundErr =
      "We could not find the program you were looking for. It might have been"++
      " deleted or probably never existed."
    execEnvNotFoundErr =
      "Unexpectedly, we could not find the associated execution environment i"++
      "n our database."

-- Returns a controller that displays the interactive editor (SmapIE) in
-- general.
-- @param mProg         - the program that will be opened (if given)
-- @param execEnv       - the associated execution environment
-- @param mExecRes      - the initial execution result 
-- @param initCode      - the initial content of the source code field
-- @param initSystemKey - the initially selected execution system
showSmapIE
  :: Maybe Program -> ExecEnv -> Maybe ExecResult -> String -> String 
  -> Controller
showSmapIE mProg execEnv mExecRes initCode initSystemKey =
  checkAuthorization (smapIEOperation $ ShowSmapIE mProg) $ \authzData ->
  return $ smapIE mProg execEnv mExecRes initCode initSystemKey
                  doExecuteProgram tryShowProgramCreationForm
                  tryShowVersionCreationForm authzData

-- Returns a controller that displays a WUI form to create a new program if the
-- given code executes successfully with the choosen execution system.
-- Otherwise, an error message will be displayed.
-- @param ctrlData - execution environment, execution system and source code 
tryShowProgramCreationForm :: (ExecEnv,String,String) -> Controller
tryShowProgramCreationForm (execEnv@(lang,systems),execSystemKey,code) =
  checkAuthorization (smapIEOperation CreateProgram) $ \authzData ->
  do execRes <- execute code lang execSystem
     case execRes of
       ExecSuccess _ -> -- execution successful
         do mUser <- maybe (return Nothing) getUserByName $ mUserName authzData
            maybe (showStdErrorPage userNotFoundErr)
                  (\user -> return $ programCreationForm lang user code
                                       (doCreateProgramCtrl execRes)
                                       (showSmapIE 
                                         Nothing execEnv (Just execRes) code
                                         execSystemKey))
                  mUser
       ExecError   _ -> -- execution failed
         do setAlert executionFailedErrAlert
            return $ smapIE Nothing execEnv (Just execRes) code execSystemKey
                            doExecuteProgram tryShowProgramCreationForm
                            tryShowVersionCreationForm authzData
  where
    execSystem = getExecSystem execSystemKey systems
    mUserName  = getUsernameFromAuthZData
    doCreateProgramCtrl execRes = doCreateProgram
      (\prog -> showSmapIE (Just prog) execEnv (Just execRes) code execSystemKey 
      ,Just programCreationSucceededErrAlert)
    userNotFoundErr =
      "Unexpectedly, no user was found with this username."
    executionFailedErrAlert = 
      ErrorAlert $"<strong>Execution failed!</strong> Only executable source "++
      "code can be added to Smap. Check the execution result panel, debug you"++
      "r code and try again!"
    programCreationSucceededErrAlert =
      SuccessAlert $"<strong>Yippee!</strong> Your program was successfully s"++
      "aved and is now available on Smap. Choose <code>\"Options > View in Br"++
      "owser\"</code> to view the Browser page of your program."

-- Returns a controller that displays a WUI form to create a new version if the
-- given code executes successfully with the choosen execution system.
-- Otherwise, an error message will be displayed.
-- @param ctrlData - execution environment, system key, source code and program
tryShowVersionCreationForm :: (ExecEnv,String,String,Program) -> Controller
tryShowVersionCreationForm (execEnv@(lang,systems),execSystemKey,code,prog) =
  checkAuthorization (smapIEOperation $ CreateVersion prog) $ \authzData ->
  do execRes <- execute code lang execSystem
     case execRes of
       ExecSuccess _ -> -- execution successful
         return $ versionCreationForm number code prog 
                                      (doCreateProgramVersionCtrl execRes)
                                      (showSmapIE 
                                        (Just prog) execEnv (Just execRes) code
                                        execSystemKey)
       ExecError   _ -> -- execution failed
         do setAlert executionFailedAlert
            return $ smapIE (Just prog) execEnv (Just execRes) code
                            execSystemKey doExecuteProgram
                            tryShowProgramCreationForm
                            tryShowVersionCreationForm authzData
  where
    number     = 1 + (length $ programVersions prog)
    execSystem = getExecSystem execSystemKey systems
    doCreateProgramVersionCtrl _ = doCreateProgramVersion
      -- (const $ showSmapIE (Just prog) execEnv (Just execRes) code execSystemKey
      (const $ showProgramInSmapIE $ programKey prog
      ,Just programVersionCreationSucceededErr)
    executionFailedAlert = 
      ErrorAlert $"<strong>Execution failed!</strong> Only executable source "++
      "code can be added to Smap. Check the execution result panel, debug you"++
      "r code and try again!"
    programVersionCreationSucceededErr =
      SuccessAlert $"<strong>Hooray!</strong> You successfully added a new ve"++
      "rsion to your program. Previous versions can be viewed in the Browser "++
      "- just choose a version from the select menu in the version panel on t"++
      "he left!"

-- Executes source code with a given execution system and returns to the IE with
-- the execution result.
-- @param execData - execution environment, source code, system key and program
doExecuteProgram :: (ExecEnv,String,String,Maybe Program) -> Controller
doExecuteProgram (execEnv@(lang,systems),execSystemKey,code,mProg) =
  checkAuthorization (smapIEOperation $ ExecuteProgram) $ \authzData ->
  do execRes <- execute code lang $ getExecSystem execSystemKey systems
     return $ smapIE mProg execEnv (Just execRes) code execSystemKey
                     doExecuteProgram tryShowProgramCreationForm
                     tryShowVersionCreationForm authzData

--------------------------------------------------------------------------------
-- Auxiliary functions                                                        --
--------------------------------------------------------------------------------

-- Returns the system with a specific key (in string representation) from a list
-- of systems. Returns `Nothing` if no such system exists.
-- @param execSystemKeyStr - the key of the system (as a string)
-- @param systems          - the list of systems
getExecSystem :: String -> [System] -> Maybe System
getExecSystem execSystemKeyStr = 
  listToMaybe . filter ((execSystemKeyStr==) . showSystemKey)

--------------------------------------------------------------------------------