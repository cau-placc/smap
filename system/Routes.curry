--------------------------------------------------------------------------------
--- This module defines operations to support the handling of routes to 
--- controllers.
---
--- (Note: This module was slightly modified to make `getControllerReference`
--- take an argument of type `Url` (specified in the module `Url`) instead of 
--- type `String`.)
---
--- @author Michael Hanus, Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module Routes (
  getControllerReference
) where

import Html5

import RoutesData
import Url

--------------------------------------------------------------------------------
-- Getting controller references                                              --
--------------------------------------------------------------------------------

--- Gets the reference of a controller corresponding to a given URL according 
--- to the definition of all routes specified in module `RoutesData`.
--- @param url - the current URL
getControllerReference :: Url -> IO (Maybe ControllerReference)
getControllerReference (path,_) = getRoutes >>= return . findControllerReference
  where
    fstPathComp = if null path then "" else head path
    findControllerReference ((matcher,fktRef):restRoutes) =
      case matcher of
        Exact string -> if (fstPathComp == string)
                        then Just fktRef
                        else findControllerReference restRoutes
        Prefix pre _ -> if (fstPathComp == pre)
                        then Just fktRef
                        else findControllerReference restRoutes
        Matcher fkt  -> if (fkt fstPathComp)
                        then Just fktRef
                        else findControllerReference restRoutes
        Always       -> Just fktRef
    findControllerReference [] = Nothing -- no controller found for URL

--------------------------------------------------------------------------------