--- This module defines an operation to generate an URL to upload
--- a program into Smap.

module GenerateProgramURL(generateUploadURL) where

import HTML(string2urlencoded)
import System(system)

-- The base url of Smap:
smapBaseUrl = "http://www-ps.informatik.uni-kiel.de/smap/smap.cgi"
--smapBaseUrl = "http://localhost/mh/smap/smap.cgi"

--- Generate URL for a given program.
--- @param uploadlanguage - the programming language for uploaded program
--- @param program - the program to be uploaded
generateUploadURL :: String -> String -> String
generateUploadURL uploadlanguage program =
  smapBaseUrl ++ "?upload?lang=" ++ uploadlanguage
              ++ "&program=" ++ string2urlencoded program

-- for testing...
test lang program
  = system ("remote-netscape \"" ++ generateUploadURL lang program ++"\"")
