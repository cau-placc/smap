--- This module defines an operation to generate an URL to upload
--- a program into Smap.

module GenerateProgramURL(generateUploadURL) where

import System.Process ( system )

import Network.URL    ( string2urlencoded )

import Config.Smap    ( smapBaseUrl )

--- Generate URL for a given program.
--- @param uploadlanguage - the programming language for uploaded program
--- @param program - the program to be uploaded
generateUploadURL :: String -> String -> String
generateUploadURL uploadlanguage program =
  smapBaseUrl ++ "?upload?lang=" ++ uploadlanguage
              ++ "&program=" ++ string2urlencoded program

-- for testing...
test :: String -> String -> IO Int
test lang program =
  system ("remote-netscape \"" ++ generateUploadURL lang program ++"\"")
