--------------------------------------------------------------------------------
--- This module defines controller references and the association of such
--- references to URLs. The mapping of controller references to actual
--- controllers is specified in the module `ControllerMapping`.
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module Config.RoutesData (
  ControllerReference(..),UrlMatch(..),Route,
  getRoutes
) where

--------------------------------------------------------------------------------
-- Controller references, URL matches and routes                              --
--------------------------------------------------------------------------------

--- This data type defines a refenence for every module in `controllers` and is
--- used for mapping URLs to actual controller functions.
data ControllerReference
  = StaticController | SmapIEController | BrowserController | AuthNController
  | AdminController

--- Definition of URL match types. Only `Exact` matches are used in this
--- application.
data UrlMatch
  = Exact String | Prefix String String | Matcher (String -> Bool) | Always

--- Type synonym for routes (simplified as compared to Spicey's standard). A
--- route consists of
--- - an URL match type
--- - a controller reference to be returned if the URL matches.
type Route
  = (UrlMatch,ControllerReference)

--------------------------------------------------------------------------------
-- Associating of URLs to controllers                                         --
--------------------------------------------------------------------------------

--- This constant specifies the association of URLs to controllers. Controllers
--- are identified here by constants of type `ControllerReference`. The actual
--- mapping of these constants into the controller operations is specified in
--- the module `ControllerMapping`.
getRoutes :: IO [Route]
getRoutes = return
  [(Exact ""         ,StaticController )
  ,(Exact "help"     ,StaticController )
  ,(Exact "about"    ,StaticController )
  ,(Exact "browser"  ,BrowserController)
  ,(Exact "signup"   ,AuthNController  )
  ,(Exact "signin"   ,AuthNController  )
  ,(Exact "signout"  ,AuthNController  )
  ,(Exact "forgot"   ,AuthNController  )
  ,(Exact "passwd"   ,AuthNController  )
  ,(Exact "languages",AdminController  )
  ,(Exact "systems"  ,AdminController  )
  ,(Exact "users"    ,AdminController  )
  ,(Exact "new"      ,SmapIEController )
  ,(Exact "upload"   ,SmapIEController )
  ,(Always           ,SmapIEController )]

--------------------------------------------------------------------------------