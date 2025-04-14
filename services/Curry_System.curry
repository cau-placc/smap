--------------------------------------------------------------------------------
-- Implementation of a simple web service to execute Curry programs with
-- some Curry system, like PAKCS or KiCS2.
--------------------------------------------------------------------------------

import Control.Monad    ( when )
import Data.Char        ( toLower, toUpper )
import Data.List
import System.Directory
import System.FilePath
import System.IO
import System.IOExts    ( evalCmd )
import System.Process   ( getPID, system )

import Network.URL      ( urlencoded2string )

import SimpleWebService ( runServiceAsCGI )

--------------------------------------------------------------------------------
-- Installing the web service                                                 --
--------------------------------------------------------------------------------

--- The main I/O action which executes the web service to execute
--- Curry programs.
main :: IO ()
main = runServiceAsCGI executeWithCurry

--------------------------------------------------------------------------------
-- Paths and parameters                                                       --
--------------------------------------------------------------------------------

-- The kind of Curry system to be used.
data CurrySystem = PAKCS | KiCS2
 deriving (Eq, Read, Show)

-- The Curry system in lowercase letters.
currySystemLC :: CurrySystem -> String
currySystemLC = map toLower . show

-- The Curry system in uppercase letters.
currySystemUC :: CurrySystem -> String
currySystemUC = map toUpper . show

--- Root of the Curry system.
currySystemHome :: CurrySystem -> String -> String
currySystemHome PAKCS version = "/opt/pakcs/pakcs-" ++ version
currySystemHome KiCS2 version = "/opt/kics2/kics2-" ++ version

--- Paths to binaries of the Curry system.
curryBinDir :: CurrySystem -> String -> String
curryBinDir currysys version = currySystemHome currysys version </> "bin"

--- Paths to libraries of the Curry system.
curryLib :: CurrySystem -> String -> String
curryLib currysys version = currySystemHome currysys version </> "lib"

--- Executable of the Curry system.
curryExec :: CurrySystem -> String -> String
curryExec cs version = curryBinDir cs version </> currySystemLC cs

--- Executable of the Curry system's front-end.
curryFrontend :: CurrySystem -> String -> String
curryFrontend cs version =
  curryBinDir cs version </> (currySystemLC cs ++ "-frontend")

-- A CPM bin directory appropriate for the Curry system and version
cpmBin :: CurrySystem -> String -> String
cpmBin PAKCS _ = "/net/medoc/home/pakcs/.cpm/bin_pakcs"
cpmBin KiCS2 _ = "/net/medoc/home/kics2/.cpm/bin_kics2"

--- Timeout command.
timeout :: String
timeout = "/usr/bin/timeout"

--- Parameters for execution with the Curry system.
currySystemParams :: CurrySystem -> [String]
currySystemParams PAKCS =
  ["-Dcurryextensions=yes"
  ,"--quiet"
  ,"--nocypm"
  ,":set","v0"
  --,":set","-verbose"
  ,":set","+time"
  ,":set","printdepth 0"
  ,":set","-interactive"]
currySystemParams KiCS2 = ["--nocypm", ":set", "v0", ":set", "+time"]

--- Time limit for execution with the Curry system.
timeLimit :: CurrySystem -> String
timeLimit PAKCS = "15"
timeLimit KiCS2 = "30"

--------------------------------------------------------------------------------
-- Execution with a Curry system (PAKCS or KiCS2)                             --
--------------------------------------------------------------------------------

--- Executes a Curry program and returns an I/O action that contains
--- the exit status (first line) and the execution output/error (rest) as plain
--- text.
--- @param urlparam - first argument: name of the Curry system (PAKCS, KiCS2);
---                   second argument: version number;
---                   third argument (optional): if "all", show all solutions
--- @param prog - the Curry program to be executed
executeWithCurry :: String -> String -> IO String
executeWithCurry urlparam inputprog = do
  pid <- getPID
  let execDir  = "tmpCURRYEXEC_" ++ show pid
      modName  = getModuleName prog
      filename = modName <.> "curry"
      urlparams = split (\c -> c =='&' || c=='?') urlparam
      (currysys,version) = if length urlparams < 2
                             then (PAKCS,"3.8.0")
                             else (read (head urlparams), urlparams !! 1)
      allsols  = length urlparams > 2 && urlparams!!2 == "all"
      prog = if null inputprog && not (null urlparams)
               then urlencoded2string (last urlparams)
               else inputprog
      shFile   = "./CURRYCALL.sh"
  currDir <- getCurrentDirectory
  createDirectoryIfMissing True execDir
  when (currysys == PAKCS) $ copyPAKCSStandardLibs version execDir
  setCurrentDirectory execDir
  writeFile filename prog
  writeFile shFile
            ("#!/bin/sh\n"++
             unwords [ addBinPath currysys version
                     , curryFrontend currysys version
                     , if currysys == PAKCS then "--flat"
                                            else "--type-annotated-flat"
                     , "-o" -- output file, e.g., `.curry/pakcs-3.6.0`
                     , ".curry/" ++ currySystemLC currysys ++ "-" ++ version
                     , "-D__" ++ currySystemUC currysys ++ "__=" ++
                       versionAsCPP version
                     , "-i", curryLib currysys version, modName])
  (exit1,out1,err1) <- evalCmd "/bin/sh" [shFile] ""
  if exit1 > 0
    then do setCurrentDirectory currDir
            system $ "/bin/rm -r " ++ execDir
            return $ parseResult currysys (exit1,out1,err1)
    else do writeFile shFile
              ("#!/bin/sh\n" ++
               unwords ([addBinPath currysys version
                        ,curryExec currysys version]
                        ++ currySystemParams currysys ++
                        [":set " ++
                         (if allsols then "-" else "+") ++ "first",
                         ":set safe",":load",modName,
                         ":eval","main",":quit"]))
            system $ "chmod 755 " ++ shFile
            result <- evalCmd timeout [timeLimit currysys, shFile] ""
            setCurrentDirectory currDir
            system $ "/bin/rm -r " ++ execDir
            return $ parseResult currysys result
 where
  -- add the Curry system bin directory and a CPM bin directory to the path
  addBinPath cs v = "PATH=" ++ curryBinDir cs v ++ ":" ++ cpmBin cs v ++
                    ":$PATH && export PATH && "

  versionAsCPP vs = case splitOn "." vs of
    (maj:min:_) -> case (reads maj, reads min) of
                     ([(ma,"")], [(mi,"")]) -> show ((ma*100+mi) :: Int)
                     _                      -> "100"
    _           -> "100" -- some default

--- Copies some standard libraries (e.g., for set functions, default rules)
--- to the given directory. The first argument is the version number of
--- the PAKCS system to be used (e.g., `"3.6.0"`).
copyPAKCSStandardLibs :: String -> String -> IO ()
copyPAKCSStandardLibs vers dir = do
  c <- system $ "/bin/cp -a " ++ "curry_libs_pakcs_" ++ vers ++ "/* " ++ dir
  when (c > 0) $ hPutStrLn stderr "Cannot copy standard libraries!"

--- Turns the result of the PAKCS execution into the proper plain text
--- representation.
--- @param result - exit status, stdin content and stderr content
parseResult :: CurrySystem -> (Int,String,String) -> String
parseResult cs (exit,out,err)
  | exit == 0   = show exit ++ "\n" ++ dropWarning out ++ err
  | exit == 2   =           "0\n" ++ dropWarning out ++ err -- no more solutions
  | exit == 124 = "124\nTIME OUT (after " ++ timeLimit cs ++ " seconds)!"
  | otherwise   = show exit ++ "\n" ++
                  "ERROR (exit code: " ++ show exit ++ ")\n" ++ out ++
                  dropWarning err
  where
    dropWarning res =
      if "Warning" `isPrefixOf` res
      then drop 1 $ dropWhile (/='\n') res
      else res

--- Gets the module name from a Curry program. If no explicit module name is
--- specified, the module name `"Main"` is returned.
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
    isSpace c        = c==' ' || c=='\n' || c=='\r' || c=='\t'
    dropLine         = drop 1 . dropWhile (/='\n')

--------------------------------------------------------------------------------
