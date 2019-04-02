--------------------------------------------------------------------------------
-- Implementation of a simple web service to execute Curry programs with KiCS2
--------------------------------------------------------------------------------

import Directory
import FilePath
import IOExts
import List
import System

import HTML.Base        ( urlencoded2string )
import SimpleWebService ( runServiceAsCGI )

--------------------------------------------------------------------------------
-- Installing the web service                                                 --
--------------------------------------------------------------------------------

--- The main I/O action which executes the web service.
main :: IO ()
main = runServiceAsCGI executeWithKiCS2

--------------------------------------------------------------------------------
-- Paths and parameters                                                       --
--------------------------------------------------------------------------------

--- Paths to required binaries and libraries.
kics2Home :: String -> String
kics2Home version = "/opt/kics2/kics2-" ++ version

kics2Bin :: String -> String
kics2Bin version = kics2Home version </> "bin"

kics2Lib :: String -> String
kics2Lib version = kics2Home version </> "lib"

kics2 :: String -> String
kics2 version = kics2Bin version </> "kics2"

kics2Frontend :: String -> String
kics2Frontend version = kics2Bin version </> "kics2-frontend"

timeout :: String
timeout  = "/usr/bin/timeout"

--- Parameters for execution with KiCS2.
kics2Params :: [String]
kics2Params = ["--nocypm", ":set", "v0", ":set", "+time"]

--- Time limit for execution with KiCS2.
timeLimit :: String
timeLimit = "30"

--------------------------------------------------------------------------------
-- Execution with KiCS2                                                       --
--------------------------------------------------------------------------------

--- Executes a Curry program with KiCS2 and returns an I/O action that contains
--- the exit status (first line) and the execution output/error (rest) as plain
--- text.
--- @param urlparam - first argument: version number,
---                   second argument: if "all", show all solutions
--- @param prog - the Curry program to be executed
executeWithKiCS2 :: String -> String -> IO String
executeWithKiCS2 urlparam inputprog = do
  pid <- getPID
  let execDir  = "tmpKiCS2EXEC_"++show pid
      modName  = getModuleName prog
      filename = modName <.> "curry"
      urlparams = split (\c -> c =='&' || c=='?') urlparam
      version  = if null urlparams then "0.6.0" else head urlparams
      allsols  = (not (null urlparams) && head urlparams == "all") ||
                 (length urlparams > 1 && urlparams!!1 == "all")
      prog = if null inputprog && not (null urlparams)
               then urlencoded2string (last urlparams)
               else inputprog
      shFile   = "./KiCS2CALL.sh"
  currDir <- getCurrentDirectory
  createDirectoryIfMissing True execDir
  setCurrentDirectory execDir
  writeFile filename prog
  let target = if "0." `isPrefixOf` version then "--flat" else "--typed-flat"
  writeFile shFile
            ("#!/bin/sh\n"++
             unwords [ addBinPath version
                     , kics2Frontend version
                     , target, "--extended"
                     , "-i", kics2Lib version, modName])
  (exit1,out1,err1) <- evalCmd "/bin/sh" [shFile] ""
  if exit1 > 0
    then do setCurrentDirectory currDir
            system $ "/bin/rm -r "++execDir
            return $ parseResult (exit1,out1,err1)
    else do writeFile shFile
              ("#!/bin/sh\n"++
               unwords ([addBinPath version, kics2 version]
                        ++ kics2Params++
                        [":set " ++
                         (if allsols then "-" else "+") ++ "first",
                         ":set safe",
                         ":load",modName,":eval","main",":quit"]))
            system $ "chmod 755 "++shFile
            result <- evalCmd timeout [timeLimit,shFile] ""
            setCurrentDirectory currDir
            system $ "/bin/rm -r "++execDir
            return $ parseResult result
 where
   -- add the Curry system bin directory to the path
   addBinPath v = "PATH=" ++ kics2Bin v ++ ":$PATH && export PATH && "

--- Turns the result of the PAKCS execution into the proper plain text
--- representation.
--- @param result - exit status, stdin content and stderr content
parseResult :: (Int,String,String) -> String
parseResult (exit,out,err)
  | exit == 0   = show exit++"\n"++dropWarning out++err
  | exit == 2   =           "0\n"++dropWarning out -- no more solutions
  | exit == 124 = "124\nTIME OUT (after "++timeLimit++" seconds)!"
  | otherwise   = show exit++"\n"++
                  "ERROR (exit code: "++show exit++")\n"++out++dropWarning err
  where
    dropWarning res =
      if "Warning" `isPrefixOf` res
      then drop 1 $ dropWhile (/='\n') res
      else res

--- Gets the module name from a Curry program. If no explicit module name is
--- specified the module name `"Main"` is returned.
--- @param prog - the Curry program
getModuleName :: String -> String
getModuleName prog =
  if "--" `isPrefixOf` progWOLeadingWSP
  then getModuleName $ dropLine prog
  else if "module" `isPrefixOf` progWOLeadingWSP
       then fst $ break (==' ') $ drop 1 $ snd $ break (==' ') progWOLeadingWSP
       else "Main"
  where
    progWOLeadingWSP = dropWhile isSpace prog
    isSpace c        = c==' '||c=='\n'||c=='\r'||c=='\t'
    dropLine         = drop 1 . dropWhile (/='\n')

--------------------------------------------------------------------------------
