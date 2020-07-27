--------------------------------------------------------------------------------
--- Library for Bootstrap 3 components (work in progress).
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module HTML.Bootstrap3 (
  ColClass(..),
  container,row,col,controlLabel,caret,
  glyphicon,navbarHeader,navbarBrand,labelDefault,labelInfo,labelWarning,badge,
  jumbotron,pageHeader,panelDefault,panelHeading,panelBody,panelFooter,
  modal,modalDialog,modalContent,modalHeader,modalBody,modalFooter,modalDismiss,
  modalToggle,stdModal,dropdownToggle,tooltipToggle,collapseToggle,
  withFade,
  parentId,targetClass,targetId,withLoadingText
) where

import Char
import List

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

container :: [HtmlExp] -> HtmlExp
container = HtmlStruct "div" [("class","container")]

row :: [HtmlExp] -> HtmlExp
row = HtmlStruct "div" [("class","row")]

col :: [ColClass] -> [HtmlExp] -> HtmlExp
col ccs = HtmlStruct "div" [("class",colClasses)]
  where
    colClasses = unwords $ map showColClass ccs
    showColClass cc = 
      let ccStr      = show cc -- TODO: make code independent of show output
          (cons,arg) = break (==' ') ccStr
       in "col" ++ parseColClassCons cons ++ "-" ++ drop 1 arg
    parseColClassCons = 
      concatMap (\c -> (if isUpper c then "-" else "")++[toLower c])

controlLabel :: String -> [HtmlExp] -> HtmlExp
controlLabel refId = HtmlStruct "label" 
  [("class","control-label")
  ,("for",refId)]

caret :: HtmlExp
caret = HtmlStruct "span" [("class","caret")] []

--------------------------------------------------------------------------------
-- Components (http://getbootstrap.com/components/)                           --
--------------------------------------------------------------------------------

glyphicon :: String -> HtmlExp
glyphicon name = HtmlStruct "span" [("class","glyphicon glyphicon-"++name)] []

navbarHeader :: [HtmlExp] -> HtmlExp
navbarHeader = HtmlStruct "div" [("class","navbar-header")]

navbarBrand :: String -> [HtmlExp] -> HtmlExp
navbarBrand url = HtmlStruct "a" [("href",url),("class","navbar-brand")]

labelDefault :: [HtmlExp] -> HtmlExp
labelDefault = HtmlStruct "span" [("class","label label-default")]

labelInfo :: [HtmlExp] -> HtmlExp
labelInfo = HtmlStruct "span" [("class","label label-info")]

labelWarning :: [HtmlExp] -> HtmlExp
labelWarning = HtmlStruct "span" [("class","label label-warning")]

badge :: [HtmlExp] -> HtmlExp
badge = HtmlStruct "span" [("class","badge")]

jumbotron :: [HtmlExp] -> HtmlExp
jumbotron = HtmlStruct "div" [("class","jumbotron")]

pageHeader :: [HtmlExp] -> HtmlExp
pageHeader = HtmlStruct "div" [("class","page-header")]

panelDefault :: [HtmlExp] -> HtmlExp
panelDefault = HtmlStruct "div" [("class","panel panel-default")]

panelHeading :: [HtmlExp] -> HtmlExp
panelHeading = HtmlStruct "div" [("class","panel-heading")]

panelBody :: [HtmlExp] -> HtmlExp
panelBody = HtmlStruct "div" [("class","panel-body")]

panelFooter :: [HtmlExp] -> HtmlExp
panelFooter = HtmlStruct "div" [("class","panel-footer")]

--------------------------------------------------------------------------------
-- Javascript (http://getbootstrap.com/javascript/)                           --
--------------------------------------------------------------------------------

-- Modal

modal :: String -> String -> [HtmlExp] -> HtmlExp
modal modalId labelId = HtmlStruct "div"
  [("class","modal")
  ,("id",modalId)
  ,("tabindex","-1")
  ,("role","dialog")
  ,("aria-labelledby",labelId)
  ,("aria-hidden","true")]

modalDialog :: [HtmlExp] -> HtmlExp
modalDialog = HtmlStruct "div" [("class","modal-dialog")]

modalContent :: [HtmlExp] -> HtmlExp
modalContent = HtmlStruct "div" [("class","modal-content")]

modalHeader :: [HtmlExp] -> HtmlExp
modalHeader = HtmlStruct "div" [("class","modal-header")]

modalBody :: [HtmlExp] -> HtmlExp
modalBody = HtmlStruct "div" [("class","modal-body")]

modalFooter :: [HtmlExp] -> HtmlExp
modalFooter = HtmlStruct "div" [("class","modal-footer")]

modalDismiss :: (String,String)
modalDismiss = ("data-dismiss","modal")

modalToggle :: (String,String)
modalToggle = ("data-toggle","modal")

stdModalClose :: HtmlExp
stdModalClose = HtmlStruct "button"
  [("type","button")
  ,("class","close")
  ,modalDismiss
  ,("aria-hidden","true")]
  [HtmlText "&times;"]

stdModal :: String -> String -> [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> HtmlExp
stdModal modalId labelId title body footer =
  modal modalId labelId `withFade`
    [modalDialog
      [modalContent
        [modalHeader
          [stdModalClose
          ,HtmlStruct "h4" [("class","modal-title")] title `addId` labelId]
        ,modalBody
          body
        ,modalFooter
          footer]]]

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

withFade :: ([HtmlExp] -> HtmlExp) -> [HtmlExp] -> HtmlExp
withFade hElem = hElem `withAddClass` "fade"

parentId :: String -> (String,String)
parentId elemId = ("data-parent",'#':elemId)

targetClass :: String -> (String,String)
targetClass elemClass = ("data-target",'.':elemClass)

targetId :: String -> (String,String)
targetId elemId = ("data-target",'#':elemId)

loadingText :: String -> (String,String)
loadingText lText = ("data-loading-text",lText)

withLoadingText :: ([HtmlExp] -> HtmlExp) -> String -> ([HtmlExp] -> HtmlExp)
withLoadingText hElem lText hExps = hElem hExps `addAttr` (loadingText lText)

--------------------------------------------------------------------------------

addToAttr :: HtmlExp -> (String,String) -> HtmlExp
addToAttr (HtmlText text)            _     = HtmlText text
addToAttr (HtmlStruct tag ats hexps) (n,v) = 
  HtmlStruct tag (addToAttr' ats) hexps
  where addToAttr' []                         = [(n,v)]
        addToAttr' ((n',v'):ats') | n' == n   = (n',v'++" "++v):ats'
                                  | otherwise = (n',v'):(addToAttr' ats')
addToAttr (HtmlCRef hexp cref)       at    = HtmlCRef  (addToAttr hexp at) cref
addToAttr (HtmlEvent hexp cref hdlr) at    =
  HtmlEvent (addToAttr hexp at) cref hdlr
addToAttr (HtmlAction act) _               = HtmlAction act

addToClass :: HtmlExp -> String -> HtmlExp
addToClass hexp cvalue = addToAttr hexp ("class",cvalue)

withAddClass :: ([HtmlExp] -> HtmlExp) -> String -> ([HtmlExp] -> HtmlExp)
withAddClass helem cls hexps = helem hexps `addToClass` cls

--------------------------------------------------------------------------------