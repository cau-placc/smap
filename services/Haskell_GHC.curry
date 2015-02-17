------------------------------------------------------------------------------
-- Web service to execute Haskell programs
------------------------------------------------------------------------------

import Directory
import FilePath
import IOExts
import List
import System
import SimpleWebService(runServiceAsCGI)

------------------------------------------------------------------------------
-- Installing the web service                                                 
------------------------------------------------------------------------------

--- The main I/O action which executes the web service.
main :: IO ()
main = runServiceAsCGI executeWithGHC

--------------------------------------------------------------------------------
-- Paths and parameters                                                       --
--------------------------------------------------------------------------------

--- Paths to required binaries.
runghc :: String
runghc   = "/opt/ghc/bin/runghc"

timeout :: String
timeout  = "/usr/bin/timeout"

--- Parameters for execution with GHC.
ghcParams :: [String]
ghcParams = ["--quiet"]

--- Time limit for execution with GHC.
timeLimit :: String
timeLimit = "5"

--------------------------------------------------------------------------------
-- Execution with GHC                                                       --
--------------------------------------------------------------------------------

--- Executes a Haskell program with GHC and returns an I/O action that contains
--- the exit status (first line) and the execution output/error (rest) as plain
--- text.
--- @param urlparameter - ignored
--- @param prog - the Haskell program to be executed
executeWithGHC :: String -> String -> IO String
executeWithGHC _ prog =
  do pid <- getPID
     let execDir  = "tmpGHCEXEC_"++show pid
         modName  = findModuleName prog
         fileName = maybe "Prog" id modName ++ ".hs"
         moduleHeader = maybe "module Prog where\n\n" (const "") modName
         mainProg = let mname = maybe "Prog" id modName
                     in "import qualified "++mname++
                        "\n\nmain = print "++mname++".main\n"
     currDir <- getCurrentDirectory
     createDirectoryIfMissing True execDir
     setCurrentDirectory execDir
     writeFile fileName (moduleHeader ++ prog)
     writeFile "Main.hs" mainProg
     result <- if containsUnsafe prog
               then return (1,"","Program contains unsafe operations!")
               else evalCmd timeout
                            [timeLimit,runghc,"Main.hs"]
                            ""
     setCurrentDirectory currDir
     system $ "/bin/rm -r "++execDir
     return $ parseResult result


--- Turns the result of the GHC execution into the proper plain text
--- representation.
--- @param result - exit status, stdin content and stderr content
parseResult :: (Int,String,String) -> String
parseResult (exit,out,err)
  | exit == 0   = show exit++"\n"++out -- ++err
  | exit == 1   = "1\n"++out++err
  | exit == 124 = "124\nTIME OUT (after "++timeLimit++" seconds)!"
  | otherwise   = show exit++"\n"++
                  "ERROR (exit code: "++show exit++")\n"++out++err

--- Finds the module name from a Haskell program if present.
--- @param prog - the Curry program
findModuleName :: String -> Maybe String
findModuleName prog =
  if "--" `isPrefixOf` progWOLeadingWSP
  then findModuleName $ dropLine prog
  else if "module" `isPrefixOf` progWOLeadingWSP
       then Just $ fst $ break (`elem` [' ','(']) $ drop 1 $ snd $
                                             break (==' ') progWOLeadingWSP
       else Nothing
  where
    progWOLeadingWSP = dropWhile isSpace prog
    isSpace c        = c==' '||c=='\n'||c=='\r'||c=='\t'
    dropLine         = drop 1 . dropWhile (/='\n')

--- Check whether program refers to "Unsafe":
--- @param prog - the Curry program
containsUnsafe :: String -> Bool
containsUnsafe prog =
  let us = snd (break (=='U') prog)
   in if null us
      then False
      else if "Unsafe" `isPrefixOf` us
           then True
           else containsUnsafe (tail us)

------------------------------------------------------------------------------

