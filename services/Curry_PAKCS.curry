--------------------------------------------------------------------------------
-- Implementation of a simple web service to execute Curry programs with PAKCS
--------------------------------------------------------------------------------

import Directory
import FilePath
import IOExts
import List
import System
import SimpleWebService(runServiceAsCGI)

--------------------------------------------------------------------------------
-- Installing the web service                                                 --
--------------------------------------------------------------------------------

--- The main I/O action which executes the web service to execute
--- Curry programs with PAKCS.
main :: IO ()
main = runServiceAsCGI executeWithPAKCS

--------------------------------------------------------------------------------
-- Paths and parameters                                                       --
--------------------------------------------------------------------------------

--- Paths to required binaries.
pakcsHome :: String -> String
pakcsHome version = "/opt/pakcs/pakcs-" ++ version

pakcsBin :: String -> String
pakcsBin version = pakcsHome version </> "bin"

pakcsLib :: String -> String
pakcsLib version = pakcsHome version </> "lib"

pakcs :: String -> String
pakcs version = pakcsBin version </> "pakcs"

cymake :: String -> String
cymake version = pakcsBin version </> "cymake"

timeout :: String
timeout = "/usr/bin/timeout"

--- Parameters for execution with PAKCS.
pakcsParams :: [String]
pakcsParams =
  ["-Dparsermessages=no"
  ,"-Dshowfcyload=no"
  ,"-Dshowplload=no"
  ,"-Dpakcsextensions=yes"
  ,"--quiet"
  ,":set","-verbose"
  ,":set","+time"
  ,":set","printdepth 0"
  ,":set","-interactive"]

--- Time limit for execution with PAKCS.
timeLimit :: String
timeLimit = "5"

--------------------------------------------------------------------------------
-- Execution with PAKCS                                                       --
--------------------------------------------------------------------------------

--- Executes a Curry program with PAKCS and returns an I/O action that contains
--- the exit status (first line) and the execution output/error (rest) as plain
--- text.
--- @param urlparam - first argument: version number,
---                   second argument: if "all", show all solutions
--- @param prog - the Curry program to be executed
executeWithPAKCS :: String -> String -> IO String
executeWithPAKCS urlparam prog =
  do pid <- getPID
     let execDir  = "tmpPAKCSEXEC_"++show pid
         modName  = getModuleName prog
         filename = modName <.> "curry"
         (urlp1,urlp2) = break (=='&') urlparam
         version  = if null urlparam then "1.14.0" else urlp1
         allsols  = urlparam=="all" || urlp2=="&all"
         shFile   = "./PAKCSCALL.sh"
     currDir <- getCurrentDirectory
     createDirectoryIfMissing True execDir
     setCurrentDirectory execDir
     writeFile filename prog
     writeFile shFile
               ("#!/bin/sh\n"++
                unwords [addBinPath version,
                         cymake version,
                         "--flat", "--extended",
                         "-i", pakcsLib version, modName])
     (exit1,out1,err1) <- evalCmd "/bin/sh" [shFile] ""
     if exit1 > 0
       then do setCurrentDirectory currDir
               system $ "/bin/rm -r "++execDir
               return $ parseResult (exit1,out1,err1)
       else do writeFile shFile
                 ("#!/bin/sh\n"++
                   unwords ([addBinPath version, pakcs version]
                            ++ pakcsParams ++
                            [":set " ++
                             (if allsols then "-" else "+") ++ "first",
                             ":set safe",":load",modName,
                             ":eval","main",":quit"]))
               system $ "chmod 755 "++shFile
               result <- evalCmd timeout [timeLimit,shFile] ""
               setCurrentDirectory currDir
               system $ "/bin/rm -r "++execDir
               return $ parseResult result
 where
   -- add the Curry system bin directory to the path
   addBinPath v = "PATH="++pakcsBin v++":$PATH && export PATH && "

--- Turns the result of the PAKCS execution into the proper plain text
--- representation.
--- @param result - exit status, stdin content and stderr content
parseResult :: (Int,String,String) -> String
parseResult (exit,out,err)
  | exit == 0   = show exit++"\n"++dropWarning out++err
  | exit == 2   =           "0\n"++dropWarning out++err -- no more solutions
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
