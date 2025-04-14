--- Some global configurations for Smap

module Config.Smap
 where

import System.FilePath ( (</>) )

--- Location of the directory containing private run-time data.
smapDataDir :: String
smapDataDir = "data"

--- Location of the database.
smapDB :: String
smapDB = smapDataDir </> "Smap.db"

--- Email address of mail sender.
smapEmail :: String
smapEmail = "smap@curry-lang.org"

-- The base url of Smap:
smapBaseUrl :: String
smapBaseUrl = "https://smap.curry-lang.org/smap.cgi"
--smapBaseUrl = "http://localhost/mh/smap/smap.cgi"

