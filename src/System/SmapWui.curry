--------------------------------------------------------------------------------
--- This module provides application-specific WUI components and elements. All
--- widgets in this module receive predefined and to a certain extent modifiable
--- (error) renderings. In additon, this module provides view elements for WUI
--- handling (like submit buttons). Component-specific, complex WUI
--- specifications that are build from the atomic WUI specifications and
--- combinators in this module are stored in the particular component views.
---
--- (Note: This module is based on a modified version of the library module
--- `WUI` where error renderings are allowed instead of just error messages. The
--- modified module can be found in `../lib`)
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version July 2020
--------------------------------------------------------------------------------

module System.SmapWui (
  module WUI,
  wSmapConstant,wSmapString,wSmapTextarea,wSmapSelect,wSmapSelectBool,wSmapPair,
  wSmapTriple,wSmap4Tuple,wSmap5Tuple,wSmap6Tuple,wSmap7Tuple,
  isRequired
) where

import HTML.Bootstrap3
import Char
import List
import Prelude hiding (div,span)
import Time
import WUI

import System.SmapHtml

--------------------------------------------------------------------------------
-- Application-specific WUI specifications for atomic form inputs             --
--------------------------------------------------------------------------------

--- A widget for constant values that cannot be modified rendered as a line with
--- a label and a value.
--- @param renderedLabel - HTML expressions for the label
--- @param renderValue   - a rendering for the value of the given type
wSmapConstant :: [HtmlExp] -> (a -> [HtmlExp]) -> WuiSpec a
wSmapConstant renderedLabel renderValue = wConstant $ \value ->
  div [classA "clearfix"]
    [div [classA "pull-left"]
      [label [] renderedLabel]
    ,div [classA "pull-right"] $
      renderValue value]

--- A widget for simple text fields with various rendering options.
--- @param metaInfos - icon (glyphicon name), input type and autofocus state
--- @param label     - a text for the control label
--- @param pHolder   - value of the placeholder attribute
--- @param messages  - help text and error message
wSmapString 
  :: (String,String,Bool) -> String -> String -> (String,String) 
  -> WuiSpec String
wSmapString (icon,typeVal,focus) label pHolder (help,err) = wString
  `withRendering`
    (smapStringRendering (label,icon,typeVal,focus,pHolder,help,err))
  `withErrorRendering`
    (smapStringErrorRendering (label,icon,typeVal,focus,pHolder,help,err))

--- A widget for textareas with various rendering options.
--- @param metaInfos - icon (gylphicon name), width/height and autofocus state
--- @param label     - a text for the control label
--- @param pHolder   - value of the placeholder attribute
--- @param messages  - help text and error message
wSmapTextarea 
  :: (String,(Int,Int),Bool) -> String -> String -> (String,String) 
  -> WuiSpec String
wSmapTextarea (icon,(height,width),focus) label pHolder (help,err) = 
  wTextArea (height,width)
  `withRendering`      
    (smapTextareaRendering (label,icon,focus,pHolder,help,err))
  `withErrorRendering` 
    (smapTextareaErrorRendering (label,icon,focus,pHolder,help,err))

--- A widget to select a value from a given list of values.
--- @param label    - a text for the control label
--- @param showElem - a mapping from values to strings
--- @param elems    - the list of values
wSmapSelect :: Eq a => String -> (a -> String) -> [a] -> WuiSpec a
wSmapSelect label showElem elems = wSelect showElem elems
  `withRendering` smapSelectRendering label

--- A widget to select a boolean value via an select menu.
--- @param label - a text for the control label
--- @param true  - string for selection of `True`
--- @param false - string for selection of `False`
wSmapSelectBool :: String -> String -> String -> WuiSpec Bool
wSmapSelectBool label true false = wSelectBool true false
  `withRendering` smapSelectRendering label

--- A widget combinator for pairs of values.
wSmapPair :: (Eq a, Eq b) => WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wSmapPair wa wb = wPair wa wb
  `withRendering` smapTupleRendering

-- A widget combinator for triples of values.
wSmapTriple :: (Eq a, Eq b, Eq c) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec (a,b,c)
wSmapTriple wa wb wc = wTriple wa wb wc
  `withRendering` smapTupleRendering

-- A widget combinator for 4-tuples of values.
wSmap4Tuple :: (Eq a, Eq b, Eq c, Eq d) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d 
  -> WuiSpec (a,b,c,d)
wSmap4Tuple wa wb wc wd = w4Tuple wa wb wc wd
  `withRendering` smapTupleRendering

-- A widget combinator for 5-tuples of values.
wSmap5Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
  -> WuiSpec (a,b,c,d,e)
wSmap5Tuple wa wb wc wd we = w5Tuple wa wb wc wd we
  `withRendering` smapTupleRendering

-- A widget combinator for 6-tuples of values.
wSmap6Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
  -> WuiSpec f -> WuiSpec (a,b,c,d,e,f)
wSmap6Tuple wa wb wc wd we wf = w6Tuple wa wb wc wd we wf
  `withRendering` smapTupleRendering

-- A widget combinator for 7-tuples of values.
wSmap7Tuple :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => 
     WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
  -> WuiSpec f -> WuiSpec g -> WuiSpec (a,b,c,d,e,f,g)
wSmap7Tuple wa wb wc wd we wf wg = w7Tuple wa wb wc wd we wf wg
  `withRendering` smapTupleRendering

--------------------------------------------------------------------------------
-- Renderings and error renderings                                            --
--------------------------------------------------------------------------------

-- The standard rendering for WUI text fields in this application.
-- @param renderingData - rendering data defined in the widget
smapStringRendering
  :: (String,String,String,Bool,String,String,String) -> Rendering
smapStringRendering (label,icon,tval,focus,pHolder,help,_) =
  renderSmapString ("",label,icon,tval,focus,pHolder,help,"")

-- The standard error rendering for WUI text fields in this application.
-- @param renderingData - rendering data defined in the widget
smapStringErrorRendering 
  :: (String,String,String,Bool,String,String,String) -> ErrorRendering
smapStringErrorRendering (label,icon,tval,_,pHolder,help,err) _ =
  renderSmapString (" has-error",label,icon,tval,False,pHolder,help,err)

-- Renders a WUI text field depending on user defined rendering options (also
-- handles the error rendering).
-- @param renderingData - rendering data defined in the widget
renderSmapString
  :: (String,String,String,String,Bool,String,String,String) -> [HtmlExp]
  -> HtmlExp
renderSmapString (state,label,icon,typeVal,focus,pHolder,help,err) [hExp] =
  div [classA $ "form-group"++state]
    [controlLabel (toValidId pHolder) [text label]
    ,mInputGroup 
    ,mHelpBlock]
  where 
    mInputGroup 
      | null icon = mAutofocus inputField `addToClass` "input-sm"
      | otherwise = div [classA "input-group input-group-sm"]
                      [span [classA "input-group-addon"]
                        [glyphicon icon]
                        ,mAutofocus inputField]
    mHelpBlock
      | null help = empty
      | otherwise = span [classA "help-block"] [text help]
    mAutofocus
      | focus     = (flip addAttr) autofocus
      | otherwise = id
    inputField
      | null err  = consClass "form-control" hExp
                      `addAttr`  (placeholder pHolder)
                      `addId`    (toValidId pHolder)
                      `setAttr`  ("type",typeVal)
      | otherwise = consClass "form-control with-error" hExp
                      `addAttrs` [placeholder pHolder
                                 ,tooltipToggle
                                 ,title err]
                      `addId`    (toValidId pHolder)
                      `setAttr`  ("type",typeVal)
    toValidId = map (\c -> if c==' ' then '-' else toLower c)

-- The standard rendering for WUI textareas in this application.
-- @param renderingData - rendering data defined in the widget
smapTextareaRendering :: (String,String,Bool,String,String,String) -> Rendering
smapTextareaRendering (label,icon,focus,pholder,htext,_) =
  renderSmapTextarea ("",label,icon,focus,pholder,htext,"")

-- The standard error rendering for WUI textareas in this application.
-- @param renderingData - rendering data defined in the widget
smapTextareaErrorRendering 
  :: (String,String,Bool,String,String,String) -> ErrorRendering
smapTextareaErrorRendering (label,icon,focus,pholder,htext,error) _ =
  renderSmapTextarea (" has-error",label,icon,focus,pholder,htext,error)

-- Renders a WUI textarea depending on user defined rendering options (also
-- handles the error rendering).
-- @param renderingData - rendering data defined in the widget
renderSmapTextarea
  :: (String,String,String,Bool,String,String,String) -> [HtmlExp] -> HtmlExp
renderSmapTextarea (state,label,icon,focus,pHolder,help,err) [hExp] =
 div [classA $ "form-group"++state]
    [controlLabel (toValidId pHolder) [text label]
    ,mInputGroup
    ,mHelpBlock]
  where 
    mInputGroup 
      | null icon = mAutofocus inputField `addToClass` "input-sm"
      | otherwise = div [classA "input-group input-group-sm"]
                      [span [classA "input-group-addon"]
                          [glyphicon icon]
                          ,mAutofocus inputField]
    mHelpBlock
      | null help = empty
      | otherwise = span [classA "help-block"] [text help]
    mAutofocus
      | focus     = (flip addAttr) autofocus
      | otherwise = id
    inputField
      | null err  = consClass "form-control" hExp
                      `addAttr`  (placeholder pHolder)
                      `addId`    (toValidId pHolder)
      | otherwise  = consClass "form-control with-error" hExp
                      `addAttrs` [placeholder pHolder
                                 ,tooltipToggle
                                 ,title err]
                      `addId`    (toValidId pHolder)
    toValidId = map (\c -> if c==' ' then '-' else toLower c)

-- The standard rendering of WUI specifications for select menus. 
smapSelectRendering :: String -> Rendering
smapSelectRendering label' [hExp] = 
  div [classA "form-group"]
    [label [classA "control-label"] [htxt label']
    ,consClass "form-control input-sm" hExp]

-- The standard rendering of WUI specifications for tuples.
smapTupleRendering :: Rendering
smapTupleRendering = div [classA "form-widget"]
  
--------------------------------------------------------------------------------
-- General conditions                                                         --
--------------------------------------------------------------------------------

--- A predicate that checks whether the submitted string value of a WUI form
--- field is not empty.
--- @param input - the input to be checked
isRequired :: String -> Bool
isRequired = not . null

--------------------------------------------------------------------------------