--------------------------------------------------------------------------------
--- Library for HTML 5 components (work in progress).
---
--- This library was primarily used for compatibility with HTML 5. It provides
--- smart constructors for HTML (5) elements with attributes (for quick
--- association with classes, e.g. for Bootstrap elements). In addition, it
--- provides some helper functions for assigning attributes to HTML elements.
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version October 2020
--------------------------------------------------------------------------------

module HTML.Html5 (
  module HTML.Base, HtmlAttr,
  text,empty,
  a,article,aside,b,br,button,code,div,em,fieldset,figcaption,figure,footer,
  {-form,-}h1,h2,h3,h4,h5,h6,header,hr,i,
  img,input,label,li,meta,nav,ol,option,p,
  script,section,select,selectNoRef,small,span,strong,
  text,textArea,textAreaNoRef,ul,
  alt,autofocus,checked,classA,cols,content,disabled,href,id,lang,name,onclick,
  placeholder,rel,role,rows,src,style,tabindex,target,targetBlank,title,value,
  buttonButton,resetButton,submitButton,formSubmitButton,selectInput,
  ariaHidden,ariaLabelledby,readonly,addId,

  consAttrs,consAttr,consClass,addToAttr,addToClass,setAttr,deleteAttr,
  toValidAttrValue,stringToValidAttrValue,withClass,withAddClass,withAttr,
  withAttrs,withId
) where

import Char
import List ( findIndex )
import HTML.Base hiding ( button, resetButton, code, footer {-, form-}
                        , h1, h2, h3, h4, h5, h6
                        , header, href, nav, section, strong, style, textArea )
import qualified HTML.Base ( textArea, button )

infixl 9 `withClass`,`withAddClass`,`withId`
infixl 0 `addToAttr`,`addToClass`,`setAttr`,`deleteAttr`,`addId`
infixr 0 `consAttrs`,`consAttr`,`consClass`

--------------------------------------------------------------------------------

type HtmlAttr = (String,String)

--------------------------------------------------------------------------------

text :: HTML h => String -> h
text = htmlText

empty :: HTML h => h
empty = htmlText ""

--------------------------------------------------------------------------------

-- HTML elements

a :: HTML h => [HtmlAttr] -> [h] -> h
a = htmlStruct "a"

article :: HTML h => [HtmlAttr] -> [h] -> h
article = htmlStruct "article"

aside :: HTML h => [HtmlAttr] -> [h] -> h
aside = htmlStruct "aside"

b :: HTML h => [HtmlAttr] -> [h] -> h
b = htmlStruct "b"

body :: HTML h => [HtmlAttr] -> [h] -> h
body = htmlStruct "body"

br :: HTML h => [HtmlAttr] -> h
br attrs = htmlStruct "br" attrs []

button :: HTML h => [HtmlAttr] -> [h] -> h
button = htmlStruct "button"

code :: HTML h => [HtmlAttr] -> [h] -> h
code = htmlStruct "code"

div :: HTML h => [HtmlAttr] -> [h] -> h
div = htmlStruct "div"

em :: HTML h => [HtmlAttr] -> [h] -> h
em = htmlStruct "em"

fieldset :: HTML h => [HtmlAttr] -> [h] -> h
fieldset = htmlStruct "fieldset"

figcaption :: HTML h => [HtmlAttr] -> [h] -> h
figcaption = htmlStruct "figcaption"

figure :: HTML h => [HtmlAttr] -> [h] -> h
figure = htmlStruct "figure"

footer :: HTML h => [HtmlAttr] -> [h] -> h
footer = htmlStruct "footer"

--form :: HTML h => [HtmlAttr] -> [h] -> h
--form = htmlStruct "form"

h1 :: HTML h => [HtmlAttr] -> [h] -> h
h1 = htmlStruct "h1"

h2 :: HTML h => [HtmlAttr] -> [h] -> h
h2 = htmlStruct "h2"

h3 :: HTML h => [HtmlAttr] -> [h] -> h
h3 = htmlStruct "h3"

h4 :: HTML h => [HtmlAttr] -> [h] -> h
h4 = htmlStruct "h4"

h5 :: HTML h => [HtmlAttr] -> [h] -> h
h5 = htmlStruct "h5"

h6 :: HTML h => [HtmlAttr] -> [h] -> h
h6 = htmlStruct "h6"

header :: HTML h => [HtmlAttr] -> [h] -> h
header = htmlStruct "header"

hr :: HTML h => [HtmlAttr] -> h
hr attrs = htmlStruct "hr" attrs []

i :: HTML h => [HtmlAttr] -> [h] -> h
i = htmlStruct "i"

img :: HTML h => [HtmlAttr] -> h
img attrs = htmlStruct "img" attrs []

input :: HTML h => [HtmlAttr] -> h
input attrs = htmlStruct "input" attrs []

label :: HTML h => [HtmlAttr] -> [h] -> h
label  = htmlStruct "label"

li :: HTML h => [HtmlAttr] -> [h] -> h
li = htmlStruct "li"

meta :: HTML h => [HtmlAttr] -> h
meta attrs = htmlStruct "meta" attrs []

nav :: HTML h => [HtmlAttr] -> [h] -> h
nav = htmlStruct "nav"

ol :: HTML h => [HtmlAttr] -> [h] -> h
ol = htmlStruct "ol"

option :: HTML h => [HtmlAttr] -> [h] -> h
option = htmlStruct "option"

p :: HTML h => [HtmlAttr] -> [h] -> h
p = htmlStruct "p"

script :: HTML h => [HtmlAttr] -> [h] -> h
script = htmlStruct "script"

section :: HTML h => [HtmlAttr] -> [h] -> h
section = htmlStruct "section"

select :: [HtmlAttr] -> HtmlRef -> [(String,String)] -> String -> HtmlExp
select attrs cRef selMenu initSelVal =
  HTML.Base.selectionInitial cRef selMenu initSel `addAttrs` attrs
 where
  initSel = maybe 0 Prelude.id (findIndex (==initSelVal) (map snd selMenu))
{- OLD:
select attrs cRef selMenu initSelVal | cRef =:= HtmlRef ref =
  HtmlInput (HtmlStruct "select" ((name ref):attrs) options) cRef
  where
    ref free
    options  = map (\(n,v) -> option ((value v):(mSel v)) [text n]) selMenu
    mSel val = if val==initSelVal then [selected] else []
-}

selectNoRef :: HTML h => [HtmlAttr] -> [(String,String)] -> String -> h
selectNoRef attrs selMenu initSelVal =
  sI selMenu initSel `addAttrs` attrs
 where
  initSel = maybe 0 Prelude.id (findIndex (==initSelVal) (map snd selMenu))

  sI sellist sel = htmlStruct "select" [] (selOption sellist sel)
   where
    selOption [] _ = []
    selOption ((n,v):nvs) i =
      htmlStruct "option"
        ([("value",v)] ++ if i==0 then [("selected","selected")] else [])
        [htxt n] : selOption nvs (i-1)


small :: HTML h => [HtmlAttr] -> [h] -> h
small = htmlStruct "small"

span :: HTML h => [HtmlAttr] -> [h] -> h
span = htmlStruct "span"

strong :: HTML h => [HtmlAttr] -> [h] -> h
strong = htmlStruct "strong"

textArea :: [HtmlAttr] -> HtmlRef -> String -> HtmlExp
textArea attrs cRef cont =
  deleteAttr (deleteAttr (HTML.Base.textArea cRef (80,10) cont) "rows") "cols"
    `addAttrs` attrs
{- OLD:
  HtmlInput (HtmlStruct "textarea" ((name ref):attrs) [text cont]) cRef
  where ref free
-}

textAreaNoRef :: HTML h => [HtmlAttr] -> String -> h
textAreaNoRef attrs cont =
  deleteAttr (deleteAttr (tA (80,10) cont) "rows") "cols"
    `addAttrs` attrs
 where
  tA (height,width) contents =
    htmlStruct "textarea" [("rows",show height),("cols",show width)]
                          [htxt contents]

ul :: HTML h => [HtmlAttr] -> [h] -> h
ul = htmlStruct "ul"

--------------------------------------------------------------------------------

-- Complex elements

buttonButton :: HTML h => [HtmlAttr] -> [h] -> h
buttonButton attrs = button $ ("type","button"):attrs

resetButton :: HTML h => [HtmlAttr] -> [h] -> h
resetButton attrs = button $ ("type","reset"):attrs

submitButton :: [HtmlAttr] -> HtmlHandler -> [HtmlExp] -> HtmlExp
submitButton attrs hdlr btnLabel
  | idOfHtmlRef cref =:= ref -- instantiate cref argument
  = HtmlEvent cref hdlr
      (HtmlStruct "input" (("type","submit") : (name ref) : attrs) btnLabel)

 where
  cref,ref free

formSubmitButton :: [HtmlAttr] -> String -> HtmlHandler -> HtmlExp
formSubmitButton attrs btnLabel hdlr =
  consAttrs attrs (HTML.Base.button btnLabel hdlr)

selectInput :: [HtmlAttr] -> HtmlRef -> [(String,String)] -> Int -> HtmlExp
selectInput attrs cref sels initSel =
  selectionInitial cref sels initSel `addAttrs` attrs
{- OLD:
selectInput attrs cref sels initSel | cref =:= HtmlRef ref =
  HtmlInput (HtmlStruct "select" ((name ref):attrs) options) cref
  where
    ref free
    options = map (\(j,(n,v)) -> option ((value v):(mSel j)) [text n])
                  (zip [0..] sels)
    mSel j = if j==initSel then [selected] else []
-}

--------------------------------------------------------------------------------

-- Attributes

alt :: String -> HtmlAttr
alt altVal = ("alt",altVal)

ariaHidden :: HtmlAttr
ariaHidden = ("aria-hidden","true")

ariaLabelledby :: String -> HtmlAttr
ariaLabelledby val = ("aria-labelledby",val)

autofocus :: HtmlAttr
autofocus = ("autofocus","autofocus")

checked :: HtmlAttr
checked = ("checked","checked")

--- class attribute
classA :: String -> HtmlAttr
classA classVal = ("class",classVal)

cols :: Int -> HtmlAttr
cols colsVal = ("cols",show colsVal)

content :: String -> HtmlAttr
content contentVal = ("content",contentVal)

disabled :: HtmlAttr
disabled = ("disabled","disabled")

href :: String -> HtmlAttr
href hrefVal = ("href",hrefVal)

id :: String -> HtmlAttr
id idVal = ("id",idVal)

lang :: String -> HtmlAttr
lang langVal = ("lang",langVal)

name :: String -> HtmlAttr
name nameVal = ("name",nameVal)

onclick :: String -> HtmlAttr
onclick onclickVal = ("onclick",onclickVal)

placeholder :: String -> HtmlAttr
placeholder placeholderVal = ("placeholder",placeholderVal)

readonly :: HtmlAttr
readonly = ("readonly","readonly")

rel :: String -> HtmlAttr
rel relVal = ("rel",relVal)

role :: String -> HtmlAttr
role roleVal = ("role",roleVal)

rows :: Int -> HtmlAttr
rows rowsVal = ("rows",show rowsVal)

selected :: HtmlAttr
selected = ("selected","selected")

src :: String -> HtmlAttr
src srcVal = ("src",srcVal)

style :: String -> HtmlAttr
style styleVal = ("style",styleVal)

tabindex :: Int -> HtmlAttr
tabindex tabindexVal = ("tabindex",show tabindexVal)

--- Target attribute.
target :: String -> HtmlAttr
target targetval = ("target",targetval)

--- Attribute `target="_blank"`.
targetBlank :: HtmlAttr
targetBlank = target "_blank"

title :: String -> HtmlAttr
title titleVal = ("title",titleVal)

value :: String -> HtmlAttr
value valueVal = ("value",valueVal)

--------------------------------------------------------------------------------

-- Helpers for assigning attributes to HTML elements

consAttrs :: HTML h => [(String,String)] -> h -> h
consAttrs nats he = updAttrs consAttrs' he
 where
  consAttrs' [] = nats
  consAttrs' ((n,v):ats)
   | n == "type" = ((n,v):nats++ats) -- skip type attribute
   | otherwise   = (nats++(n,v):ats)

consAttr :: HTML h => (String,String) -> h -> h
consAttr nat = consAttrs [nat]

consClass :: HTML h => String -> h -> h
consClass cvalue = consAttr ("class",cvalue)

addToAttr :: HTML h => h -> (String,String) -> h
addToAttr he (n,v) = updAttrs addToAttr' he
  where addToAttr' []                         = [(n,v)]
        addToAttr' ((n',v'):ats') | n' == n   = (n',v'++" "++v):ats'
                                  | otherwise = (n',v'):(addToAttr' ats')

addToClass :: HTML h => h -> String -> h
addToClass hexp cvalue = addToAttr hexp ("class",cvalue)

setAttr :: HTML h => h -> (String,String) -> h
setAttr he (n,v) = updAttrs setAttr' he
  where setAttr' [] = [(n,v)]
        setAttr' ((n',v'):ats') | n' == n   = ((n,v):ats')
                                   | otherwise = (n',v'):(setAttr' ats') 

deleteAttr :: HTML h => h -> String -> h
deleteAttr he n = updAttrs deleteAttr' he
  where deleteAttr' [] = []
        deleteAttr' ((n',v):ats') | n' == n   = ats'
                                  | otherwise = (n',v):(deleteAttr' ats') 


toValidAttrValue :: String -> String
toValidAttrValue = map (\c -> if c==' ' then '-' else toLower c)

stringToValidAttrValue :: String -> String
stringToValidAttrValue = map (\c -> if c==' ' then '-' else toLower c)

withClass :: HTML h => ([h] -> h) -> String -> ([h] -> h)
withClass helem classVal = consClass classVal . helem

withAddClass :: HTML h => ([h] -> h) -> String -> ([h] -> h)
withAddClass helem classVal hexps = helem hexps `addToClass` classVal

withAttrs :: HTML h => ([h] -> h) -> [(String,String)] -> ([h] -> h)
withAttrs helem attrs hexps = helem hexps `addAttrs` attrs

withAttr :: HTML h => ([h] -> h) -> (String,String) -> ([h] -> h)
withAttr helem attr = withAttrs helem [attr]

withId :: HTML h => ([h] -> h) -> String -> ([h] -> h)
withId helem idVal = withAttr helem ("id",idVal)

addId :: HTML h => h -> String -> h
addId hExp idVal = addAttr hExp ("id",idVal)

--------------------------------------------------------------------------------