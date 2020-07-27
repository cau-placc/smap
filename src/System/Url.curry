--------------------------------------------------------------------------------
--- This module provides general operations for handling URLs (which are 
--- essentially the CGI parameter strings passed to the main script).
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module System.Url (
  Url,getUrl,showUrl,showPath,showQueryString,
  getIntValueFromQueryString,getStrValueFromQueryString
) where

import HTML.Base
import List
import ReadNumeric

--------------------------------------------------------------------------------

--- The general type of URLs in this application. An URL (which is essentially
--- the CGI parameter string passed to the main script) consists of
--- - a list of strings containing the components of the URL path
--- - a list containing pairs of field names and values from the query string.
type Url = ([String],[(String,String)])

--------------------------------------------------------------------------------
-- Getting and showing URLs                                                   --
--------------------------------------------------------------------------------

--- Gets the current URL from the environment.
getUrl :: IO Url
getUrl = getUrlParameter >>= return . parseUrl

--- Turns a given URL into its string representation (based on the current way
--- of passing CGI parameters; thus, a question mark is added at the beginning 
--- of the URL path).
--- @param url - the URL
showUrl :: Url -> String
showUrl url = showPath url ++ showQueryString url

--- Turns the URL path of a given URL into its string representation (based on
--- the current way of passing CGI parameters; thus, a question mark is added at
--- the beginning).
--- @param url - the URL containing the URL path
showPath :: Url -> String
showPath (path,_) =
  if null path 
    then ""
    else "?" ++ intercalate "/" path

--- Turns the query string component of a given URL into its string
--- representation. Also adds the question mark at the beginning.
--- @param url - the URL containing the query string
showQueryString :: Url -> String
showQueryString (_,qStr) =
  if null qStr 
  then "" 
  else "?"++(intercalate "&" $ map (\(field,val) -> field++"="++val) qStr)

--------------------------------------------------------------------------------
-- Getting query string parameters                                            --
--------------------------------------------------------------------------------

--- Looks up a field in a query string and returns its value as an integer.
--- Returns `Nothing` if the query string does not contain the field or if no 
--- integer value could be read from its value.
--- @param field - the field name
--- @param qStr  - the query string
getIntValueFromQueryString :: String -> [(String,String)] -> IO (Maybe Int)
getIntValueFromQueryString field qStr =
  let mValStr = lookup field qStr
   in return $ maybe Nothing
                     (\valStr -> maybe Nothing
                                       (Just . fst)
                                       (readInt valStr))
                     mValStr

--- Looks up a field in a query string and returns its value as an URL encoded
--- string. Returns `Nothing` if the query string does not contain the field.
--- @param field - the field name
--- @param qStr  - the query string
getStrValueFromQueryString :: String -> [(String,String)] -> IO (Maybe String)
getStrValueFromQueryString field qStr =
  let mValStr = lookup field qStr
   in return $ maybe Nothing
                     (Just . urlencoded2string)
                     mValStr

--------------------------------------------------------------------------------
-- Auxiliary functions for parsing URLs                                       --
--------------------------------------------------------------------------------

-- Parses the URL parameters passed to the main script and turns them into the
-- the URL representation as a pair consisting of the URL path and the query
-- string.
-- @param urlParam - the URL parameter from the environment
parseUrl :: String -> Url
parseUrl urlParam =
  let (pathStr,qStrStr) = break (=='?') urlParam
      path              = parsePath pathStr
      qStr              = parseQueryString $ drop 1 qStrStr
   in (path,qStr)

-- Parses the path passed to the main script via its URL parameters and returns
-- a list of the path components.
-- @param pathStr - the path in string representation
parsePath :: String -> [String]
parsePath pathStr =
  let (first,rest) = break (=='/') pathStr
   in if null rest
      then [first]
      else first : (parsePath $ tail rest)

-- Parses the query string passed to the main script via its URL parameters and
-- returns a list of pairs consisting of the field name and value.
-- @param qStrStr - the query string in string representation
parseQueryString :: String -> [(String,String)]
parseQueryString qStrStr =
  let (first,rest) = break (=='&') qStrStr
   in if null first
      then []
      else let (field,val) = parseQueryStringPair first
            in if null field || null val
               then parseQueryString $ drop 1 rest
               else (field,val) : (parseQueryString $ drop 1 rest)

-- Parses a single query string parameter and returns a pair consisting of the
-- field name and value.
-- @param qStrPStr - the query string parameter pair in string representation
parseQueryStringPair :: String -> (String,String)
parseQueryStringPair qStrPStr =
  let (field,valStr) = break (=='=') qStrPStr
      val            = drop 1 valStr
   in (field,val)

--------------------------------------------------------------------------------