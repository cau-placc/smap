--------------------------------------------------------------------------------
--- Library for Bootstrap 3 components (work in progress).
---
--- @author Lasse Kristopher Meyer
--- @version January 2024
--------------------------------------------------------------------------------

module HTML.Bootstrap3 (
  ColClass(..),
  container,row,col,controlLabel,caret,
  glyphicon,navbarHeader,navbarBrand,labelDefault,labelInfo,labelWarning,badge,
  jumbotron,pageHeader,panelDefault,panelHeading,panelBody,panelFooter,
  modal,modalDialog,modalContent,modalHeader,modalBody,modalFooter,modalDismiss,
  modalToggle,stdModal,scriptShowModal,
  dropdownToggle,tooltipToggle,collapseToggle,
  withFade,
  parentId,targetClass,targetId,withLoadingText
) where

import Data.Char
import Data.List

import HTML.Base
import HTML.Html5

--------------------------------------------------------------------------------

data ColClass
  = Xs Int | Sm Int | Md Int | Lg Int | XsOffset Int | SmOffset Int 
  | MdOffset Int | LgOffset Int | XsPush Int | SmPush Int | MdPush Int 
  | LgPush Int | XsPull Int | SmPull Int | MdPull Int | LgPull Int
 deriving Show

--------------------------------------------------------------------------------
-- CSS (http://getbootstrap.com/css/)                                         --
--------------------------------------------------------------------------------

container :: HTML h => [h] -> h
container = htmlStruct "div" [("class","container")]

row :: HTML h => [h] -> h
row = htmlStruct "div" [("class","row")]

col :: HTML h => [ColClass] -> [h] -> h
col ccs = htmlStruct "div" [("class",colClasses)]
  where
    colClasses = unwords $ map showColClass ccs
    showColClass cc = 
      let ccStr      = show cc -- TODO: make code independent of show output
          (cons,arg) = break (==' ') ccStr
       in "col" ++ parseColClassCons cons ++ "-" ++ drop 1 arg
    parseColClassCons = 
      concatMap (\c -> (if isUpper c then "-" else "")++[toLower c])

controlLabel :: HTML h => String -> [h] -> h
controlLabel refId = htmlStruct "label" 
  [("class","control-label")
  ,("for",refId)]

caret :: HTML h => h
caret = htmlStruct "span" [("class","caret")] []

--------------------------------------------------------------------------------
-- Components (http://getbootstrap.com/components/)                           --
--------------------------------------------------------------------------------

glyphicon :: HTML h => String -> h
glyphicon name = htmlStruct "span" [("class","glyphicon glyphicon-"++name)] []

navbarHeader :: HTML h => [h] -> h
navbarHeader = htmlStruct "div" [("class","navbar-header")]

navbarBrand :: HTML h => String -> [h] -> h
navbarBrand url = htmlStruct "a" [("href",url),("class","navbar-brand")]

labelDefault :: HTML h => [h] -> h
labelDefault = htmlStruct "span" [("class","label label-default")]

labelInfo :: HTML h => [h] -> h
labelInfo = htmlStruct "span" [("class","label label-info")]

labelWarning :: HTML h => [h] -> h
labelWarning = htmlStruct "span" [("class","label label-warning")]

badge :: HTML h => [h] -> h
badge = htmlStruct "span" [("class","badge")]

jumbotron :: HTML h => [h] -> h
jumbotron = htmlStruct "div" [("class","jumbotron")]

pageHeader :: HTML h => [h] -> h
pageHeader = htmlStruct "div" [("class","page-header")]

panelDefault :: HTML h => [h] -> h
panelDefault = htmlStruct "div" [("class","panel panel-default")]

panelHeading :: HTML h => [h] -> h
panelHeading = htmlStruct "div" [("class","panel-heading")]

panelBody :: HTML h => [h] -> h
panelBody = htmlStruct "div" [("class","panel-body")]

panelFooter :: HTML h => [h] -> h
panelFooter = htmlStruct "div" [("class","panel-footer")]

--------------------------------------------------------------------------------
-- Javascript (http://getbootstrap.com/javascript/)                           --
--------------------------------------------------------------------------------

-- Modal

modal :: HTML h => String -> String -> [h] -> h
modal modalId labelId = htmlStruct "div"
  [("class","modal")
  ,("id",modalId)
  ,("tabindex","-1")
  ,("role","dialog")
  ,("aria-labelledby",labelId)
  ,("aria-hidden","true")]

modalDialog :: HTML h => [h] -> h
modalDialog = htmlStruct "div" [("class","modal-dialog")]

modalContent :: HTML h => [h] -> h
modalContent = htmlStruct "div" [("class","modal-content")]

modalHeader :: HTML h => [h] -> h
modalHeader = htmlStruct "div" [("class","modal-header")]

modalBody :: HTML h => [h] -> h
modalBody = htmlStruct "div" [("class","modal-body")]

modalFooter :: HTML h => [h] -> h
modalFooter = htmlStruct "div" [("class","modal-footer")]

modalDismiss :: (String,String)
modalDismiss = ("data-dismiss","modal")

modalToggle :: (String,String)
modalToggle = ("data-toggle","modal")

stdModalClose :: HTML h => h
stdModalClose = htmlStruct "button"
  [("type","button")
  ,("class","close")
  ,modalDismiss
  ,("aria-hidden","true")]
  [htmlText "&times;"]

stdModal :: HTML h => String -> String -> [h] -> [h] -> [h] -> h
stdModal modalId labelId title body footer =
  modal modalId labelId `withFade`
    [modalDialog
      [modalContent
        [modalHeader
          [stdModalClose
          ,htmlStruct "h4" [("class","modal-title")] title `addId` labelId]
        ,modalBody
          body
        ,modalFooter
          footer]]]

--- A JavaScript element which can be put at the end of the page body
--- in order to show a modal defined in the page after the page is loaded.
scriptShowModal :: String -> [BaseHtml]
scriptShowModal modalid =
  [hStruct "script"
    [htmlText $ "$(document).ready(function(){ $(\"#" ++ modalid ++
                "\").modal('show'); });"
    ]
  ]

-- Dropdown

dropdownToggle :: (String,String)
dropdownToggle = ("data-toggle","dropdown")

-- Tooltip

tooltipToggle :: (String,String)
tooltipToggle = ("data-toggle","tooltip")

-- Collapse

collapseToggle :: (String,String)
collapseToggle = ("data-toggle","collapse")

--------------------------------------------------------------------------------

withFade :: HTML h => ([h] -> h) -> [h] -> h
withFade hElem = hElem `withAddClass` "fade"

parentId :: String -> (String,String)
parentId elemId = ("data-parent",'#':elemId)

targetClass :: String -> (String,String)
targetClass elemClass = ("data-target",'.':elemClass)

targetId :: String -> (String,String)
targetId elemId = ("data-target",'#':elemId)

loadingText :: String -> (String,String)
loadingText lText = ("data-loading-text",lText)

withLoadingText :: HTML h => ([h] -> h) -> String -> ([h] -> h)
withLoadingText hElem lText hExps = hElem hExps `addAttr` (loadingText lText)

--------------------------------------------------------------------------------

addToAttr :: HTML h => h -> (String,String) -> h
addToAttr he (n,v) = updAttrs addToAttr' he
  where addToAttr' []                         = [(n,v)]
        addToAttr' ((n',v'):ats') | n' == n   = (n',v'++" "++v):ats'
                                  | otherwise = (n',v'):(addToAttr' ats')

addToClass :: HTML h => h -> String -> h
addToClass hexp cvalue = addToAttr hexp ("class",cvalue)

withAddClass :: HTML h => ([h] -> h) -> String -> ([h] -> h)
withAddClass helem cls hexps = helem hexps `addToClass` cls

--------------------------------------------------------------------------------