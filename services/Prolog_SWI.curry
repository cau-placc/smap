------------------------------------------------------------------------------
-- Web service to execute Prolog programs with SWI-Prolog in sandbox mode
------------------------------------------------------------------------------

import Directory
import FilePath
import IOExts
import List
import System

import HTML.Base        ( urlencoded2string )
import SimpleWebService ( runServiceAsCGI )

------------------------------------------------------------------------------
-- Installing the web service                                                 
------------------------------------------------------------------------------

--- The main I/O action which executes the web service.
main :: IO ()
main = runServiceAsCGI executeWithSWI

--------------------------------------------------------------------------------
-- Paths and parameters                                                       --
--------------------------------------------------------------------------------

--- Paths to required binaries.
swi :: String
swi      = "/opt/swiprolog/bin/swipl"

timeout :: String
timeout  = "/usr/bin/timeout"

--- Parameters for execution with SWI.
swiParams :: [String]
swiParams = ["--quiet"]

--- Time limit for execution with SWI.
timeLimit :: String
timeLimit = "5"

--------------------------------------------------------------------------------
-- Execution with SWI                                                       --
--------------------------------------------------------------------------------

--- Executes a Prolog program with SWI and returns an I/O action that contains
--- the exit status (first line) and the execution output/error (rest) as plain
--- text.
--- @param urlparameter - contains the URL encoded program if the
---                       second parameter is empty
--- @param prog - the Prolog program to be executed
executeWithSWI :: String -> String -> IO String
executeWithSWI urlparam inputprog = do
  pid <- getPID
  let execDir  = "tmpSWIEXEC_"++show pid
      filename = "test.pl"
      prog = if null inputprog && not (null urlparam)
               then urlencoded2string urlparam
               else inputprog
  currDir <- getCurrentDirectory
  createDirectoryIfMissing True execDir
  setCurrentDirectory execDir
  writeFile filename prog
  result <- evalCmd timeout
                    ([timeLimit,swi]++swiParams)
                    ("compile('../safeload'). safe_exec('"++filename++"').")
  setCurrentDirectory currDir
  system $ "/bin/rm -r "++execDir
  return $ parseResult result


--- Turns the result of the SWI execution into the proper plain text
--- representation.
--- @param result - exit status, stdin content and stderr content
parseResult :: (Int,String,String) -> String
parseResult (exit,out,err)
  | exit == 0   = show exit++"\n"++out -- ++err
  | exit == 124 = "124\nTIME OUT (after "++timeLimit++" seconds)!"
  | otherwise   = show exit++"\n"++
                  "ERROR (exit code: "++show exit++")\n"++out++err

--------------------------------------------------------------------------------
