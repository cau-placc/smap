--------------------------------------------------------------------------------
--- This module implements the execution of programs. It primarily provides an
--- appropriate function to execute source code of a specific programming
--- language with an associated language implementation system from the
--- execution environment (see module `ExecEnv`) and a data type to classify
--- the execution results.
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version April 2025
--------------------------------------------------------------------------------

module System.Execution (
  ExecResult(..), execute
) where

import Data.List      ( last )
import Data.Time
import Numeric            ( readNat )
import System.Directory
import System.Environment ( getEnv )
import System.IO
import System.IOExts      ( connectToCommand, readCompleteFile )
import System.Process     ( getPID, system )
import System.PreludeHelpers

import Network.Socket
import Network.URL        ( string2urlencoded )

import Model.ExecEnv

--------------------------------------------------------------------------------
-- Executing programs                                                         --
--------------------------------------------------------------------------------

--- The data type für execution results. Currently, there are two different
--- types of execution results (future extensions may differentiate between more
--- types depending on execution exit codes).
--- @cons ExecSuccess out - the constructor for successful executions
--- @cons ExecError out   - the constructor for failed executions
data ExecResult = ExecSuccess String | ExecError String
 deriving (Read, Show)

--- Executes the given source code with the given language implementation
--- system and returns an `ExecResult` containing the execution output. If no
--- system is given an `ExecError` is returned.
--- @param code    - the program to be executed
--- @param mSystem - a possibly given execution system
execute :: String -> Language -> Maybe System -> IO ExecResult
execute code lang mSystem =
  maybe (return $ ExecError noSystemFoundErr)
        (\s -> do logProgram code (languageFilenameExt lang) (systemName s)
                  (exitCode,result) <- connectToCGI (systemExecUrl s) code
                  execResult        <- getExecResult exitCode (header s++result)
                  return execResult)
        mSystem
  where
    header s     = headerRule s++headerText s++headerRule s++"\n"
    headerText s = "Executing with "++systemName s++":\n"
    headerRule s = replicate (length (headerText s)-1) '-'++"\n"
    noSystemFoundErr =
      "-----------------------\nOops, an error occured!\n--------------------"++
      "---\n\nSorry, this programming language is not yet fully supported."

-- Maps exit codes from execution results to values of type `ExecResult`.
-- @param exitCode - the exit code of the execution attempt
-- @param output   - the textual output of the execution attempt (stdout/stderr)
getExecResult :: Int -> String -> IO ExecResult
getExecResult exitCode output = return $
  case exitCode of
    0 -> ExecSuccess output
    _ -> ExecError   output

--------------------------------------------------------------------------------
--- Connecting to a web service (originally by Michael Hanus)                 --
--------------------------------------------------------------------------------

-- Sends an input string to given CGI URL and retrieves the output of the script
-- execution.
-- @param url   - the URL of the execution web service
-- @param input - the source code to be executed
-- @author Michael Hanus, Lasse Kristopher Meyer
connectToCGI :: String -> String -> IO (Int,String)
connectToCGI url input = 
  maybe (return (1,"The execution service could not be reached."))
        (\(host,path,socketNr) -> 
          do response <- httpGet host path socketNr input
             return $ readResult response)
        (partitionUrl url)

-- Parses the textual result from the SCI script execution and extracts the exit
-- code abd the execution output.
-- @param postResponse - the response to the POST request as plain text
readResult :: String -> (Int,String)
readResult postResponse =
  case readNat fstLn of
    [(exitCode,_)] ->  (exitCode,drop 1 rest)
    _ -> (1,"No exit code found in answer from execution service:\n" ++
            postResponse)
 where
  (fstLn,rest) = break (=='\n') postResponse

-- An I/O action that shows the answer of a web server to the request of a
-- document with the GET method where the input is separated in the URL
-- by "&".
httpGet :: String -> String -> Int -> String -> IO String
httpGet host doc snr input = do
  let url = host ++ (if snr==80 then "" else ':' : show snr) ++
            doc ++ (if null input then "" else '?' : string2urlencoded input)
  getContentsOfUrl url
 where
  getContentsOfUrl url = do
    pid <- getPID
    let tmpfile = "/tmp/wgeturl."++show pid
    system ("wget -O "++tmpfile++" \""++url++"\" 2> /dev/null")
    cont <- readCompleteFile tmpfile
    system ("rm -f "++tmpfile)
    return cont

-- An I/O action that shows the answer of a web server to the request of a
-- document with the POST method.
httpPost :: String -> String -> Int -> String -> IO String
httpPost host doc snr input = do
  writeFile "/tmp/POSTINPUT" postInput
  str <- connectToSocket host snr
  hPutStr str postInput
  hFlush str
  result <- hGetContents str
  return $ readContent result
 where
  postInput = unlines $
    [ "POST " ++ doc ++ " HTTP/1.0"
    , "Host: " ++ host
    , "Content-Type: text/plain; charset=ISO-8859-1"
    , "Content-Length: " ++ show (length input)
    , "" -- end of HTTP header
    , input
    ]

test1 :: IO String
test1 = httpPost "www-ps.informatik.uni-kiel.de" "/~pakcs/smap/exec/PAKCS.cgi?3.6.0" 80 "main = 3*4"
test2 :: IO String
test2 = httpGet "www-ps.informatik.uni-kiel.de" "/~pakcs/smap/exec/PAKCS.cgi?3.6.0" 80 "main = 3*4"

-- Splits a URL into the host name, path, and socket number (default 80).
-- @author Michael Hanus
partitionUrl :: String -> Maybe (String,String,Int)
partitionUrl purl = let (protocol,url) = splitAt 7 purl in
  if protocol=="http://" 
  then case break (==':') url of 
         (host,_:snrPath) -> case break (=='/') snrPath of
                               (snr,"") -> result host "/" snr
                               (snr,path) -> result host path snr
         _                -> case break (=='/') url of
                               (host,"") -> Just (host,"/",80)
                               (host,path) -> Just (host,path,80)
  else Nothing                             
  where
    result host path snrs = if snr == 0 then Nothing else Just (host,path,snr)
      where
        snr = read snrs

-- Yield content (i.e., the string following the first empty line).
-- @author Michael Hanus
readContent :: String -> String
readContent s = case break (=='\n') s of
  (_,'\n':'\r':'\n':conts) -> conts
  (_,'\n':'\n':conts)      -> conts
  (_,'\n':noconts)         -> readContent noconts
  _                        -> "error: no content read"

------------------------------------------------------------------------------
-- Directory where executed programs are logged
logDir :: String
logDir = "executed_programs"

-- log every executed program:
logProgram :: String -> String -> String -> IO ()
logProgram prog fnameextension sysname = do
  ensureDirectoryExists logDir
  ct <- getLocalTime
  let progdirname = logDir ++"/"++ show (ctYear ct) ++"_"++ show (ctMonth ct)
  ensureDirectoryExists progdirname
  let fname = progdirname++"/prog" ++
              concatMap (\f->'_':show (f ct))
                        [ctYear,ctMonth,ctDay,ctHour,ctMin,ctSec] ++
              "." ++ fnameextension
  raddr <- getEnv "REMOTE_ADDR"
  rhost <- if null raddr then return "???" else getHostnameForIP raddr
  writeFile fname
            ("-- From: "++rhost++"\n"++"-- System: "++sysname++"\n"++prog++"\n")

--- Get symbolic name of ip address:
getHostnameForIP :: String -> IO String
getHostnameForIP ipaddr = (flip catch) (\_ -> return "") $ do
  h <- connectToCommand $ "host " ++ ipaddr
  b <- hIsEOF h
  if b then return ""
       else hGetLine h >>= return . last . words

ensureDirectoryExists :: String -> IO ()
ensureDirectoryExists dir = do
  exdir <- doesDirectoryExist dir
  if exdir then return () else createDirectory dir

------------------------------------------------------------------------------
