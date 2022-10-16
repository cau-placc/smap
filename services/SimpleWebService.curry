------------------------------------------------------------------------
--- This module supports the execution of I/O actions in the web
--- (so that they can be used as simple "web services")
--- Any I/O action can be installed (via `runCgiCurryExec`) as a CGI script
--- which can be accessed via the POST method.
--- The operation `connectToCGI` provides a method to connect
--- such a web services.
---
--- @author Michael Hanus
--- @version October 2022
------------------------------------------------------------------------

module SimpleWebService
  ( runServiceAsCGI, connectToCGI )
 where

import Data.Maybe         ( isJust )
import System.Environment ( getEnv )
import System.IO

import Network.Socket

------------------------------------------------------------------------
--- Installing a web service:

--- Transform an I/O action into a simple web service.
--- The I/O action takes the URL parameter and the standard input
--- as parameters and yields a string result.
--- In order to make such an I/O action web accessible,
--- it must be wrapped by this operation and the compiled program
--- must be installed as a CGI script on a web server.
--- If this CGI script is activated (by POST), it reads the contents of stdin
--- and calls the I/O action with the URL parameter and the stdin contents.
--- The computed result is returned as a plain text answer.
---
--- @param service - the function executed as a web service taking the
---                  URL parameter and stdin contents as parameters
runServiceAsCGI :: (String -> String -> IO String) -> IO ()
runServiceAsCGI service = do
  param <- getEnv "QUERY_STRING"
  clen  <- getEnv "CONTENT_LENGTH"
  cont  <- getNChars (read clen)
  result <- service param cont
  putStrLn "Content-type: text/plain"
  putStrLn ""  -- end of HTTP header
  putStr result
 where
   -- get n chars from stdin:
   getNChars n =
     if n<=0 then return ""
             else do c <- getChar
                     cs <- getNChars (n-1)
                     return (c:cs)

------------------------------------------------------------------------
--- Connecting to a web service:

--- Sends an input string to given CGI url via POST and returns the output
--- of the script execution.
connectToCGI :: String -> String -> IO String
connectToCGI url input = 
  maybe (return "Invalid URL") 
        (\ (host,docpath,portnum) -> httpPost host docpath portnum input)
        (partitionUrl url)

-- Splits a URL into the host name, path, and socket number (default 80).
partitionUrl :: String -> Maybe (String,String,Int)
partitionUrl purl = let (protocol,url) = splitAt 7 purl in
  if protocol=="http://" then case break (==':') url of 
       (host,_:pnrPath) -> case break (=='/') pnrPath of
                             (pnr,"") -> result host "/" pnr
                             (pnr,path) -> result host path pnr
       _ -> case break (=='/') url of
                   (host,"") -> Just (host,"/",80)
                   (host,path) -> Just (host,path,80)
     else Nothing
                             
  where
    result host path pnrs = if pnr == 0 then Nothing else Just (host,path,pnr)
      where
        pnr = read pnrs

--- validate given url
isValidUrl :: String -> Bool
isValidUrl = isJust . partitionUrl

-- An I/O action that shows the answer of a web server to the
-- request of a document:
httpPost :: String -> String -> Int -> String -> IO String
httpPost hostname docpath portnum input = do
 str <- connectToSocket hostname portnum
 hPutStrLn str ("POST " ++ docpath ++ " HTTP/1.0")
 hPutStrLn str ("Host: " ++ hostname)
 hPutStrLn str "Content-Type: text/plain; charset=ISO-8859-1"
 hPutStrLn str ("Content-Length: "++show (length input))
 hPutStrLn str "" -- end of HTTP header
 hPutStrLn str input
 hFlush str
 result <- hGetContents str
 return (readContent result)

-- yield content (i.e., the string following the first empty line)
readContent :: String -> String
readContent s = case break (=='\n') s of
    (_,'\n':'\r':'\n':conts) -> conts
    (_,'\n':'\n':conts) -> conts
    (_,'\n':noconts) -> readContent noconts
    _ -> "error: no content read"

------------------------------------------------------------------------
-- Example:
execURL :: String
execURL = "http://giscours.informatik.uni-kiel.de/~pakcs/smap/exec/PAKCS.cgi"

test1 :: IO ()
test1 = connectToCGI execURL "main = 3+4" >>= putStrLn

