--- Some global configurations for Smap

module Config.Smap
 where

import FilePath ( (</>) )

--- Location of the directory containing private run-time data.
smapDataDir :: String
smapDataDir = "data"

--- Location of the database.
smapDB :: String
smapDB = smapDataDir </> "Smap.db"

--- Email address of mail sender.
smapEmail :: String
smapEmail = "smap@curry-lang.org"
