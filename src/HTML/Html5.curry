--------------------------------------------------------------------------------
--- Library for HTML 5 components (work in progress).
---
--- This library was primarily used for compatibility with HTML 5. It provides
--- smart constructors for HTML (5) elements with attributes (for quick
--- association with classes, e.g. for Bootstrap elements). In addition, it
--- provides some helper functions for assigning attributes to HTML elements.
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version July 2020
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

text :: String -> HtmlExp
text = HtmlText

empty :: HtmlExp
empty = HtmlText ""

--------------------------------------------------------------------------------

-- HTML elements

a :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
a = HtmlStruct "a"

article :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
article = HtmlStruct "article"

aside :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
aside = HtmlStruct "aside"

b :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
b = HtmlStruct "b"

body :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
body = HtmlStruct "body"

br :: [HtmlAttr] -> HtmlExp
br attrs = HtmlStruct "br" attrs []

button :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
button = HtmlStruct "button"

code :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
code = HtmlStruct "code"

div :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
div = HtmlStruct "div"

em :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
em = HtmlStruct "em"

fieldset :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
fieldset = HtmlStruct "fieldset"

figcaption :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
figcaption = HtmlStruct "figcaption"

figure :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
figure = HtmlStruct "figure"

footer :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
footer = HtmlStruct "footer"

--form :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
--form = HtmlStruct "form"

h1 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h1 = HtmlStruct "h1"

h2 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h2 = HtmlStruct "h2"

h3 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h3 = HtmlStruct "h3"

h4 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h4 = HtmlStruct "h4"

h5 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h5 = HtmlStruct "h5"

h6 :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
h6 = HtmlStruct "h6"

header :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
header = HtmlStruct "header"

hr :: [HtmlAttr] -> HtmlExp
hr attrs = HtmlStruct "hr" attrs []

i :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
i = HtmlStruct "i"

img :: [HtmlAttr] -> HtmlExp
img attrs = HtmlStruct "img" attrs []

input :: [HtmlAttr] -> HtmlExp
input attrs = HtmlStruct "input" attrs []

label :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
label  = HtmlStruct "label"

li :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
li = HtmlStruct "li"

meta :: [HtmlAttr] -> HtmlExp
meta attrs = HtmlStruct "meta" attrs []

nav :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
nav = HtmlStruct "nav"

ol :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
ol = HtmlStruct "ol"

option :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
option = HtmlStruct "option"

p :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
p = HtmlStruct "p"

script :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
script = HtmlStruct "script"

section :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
section = HtmlStruct "section"

select :: [HtmlAttr] -> CgiRef -> [(String,String)] -> String -> HtmlExp
select attrs cRef selMenu initSelVal =
  HTML.Base.selectionInitial cRef selMenu initSel `addAttrs` attrs
 where
  initSel = maybe 0 Prelude.id (findIndex (==initSelVal) (map snd selMenu))
{- OLD:
select attrs cRef selMenu initSelVal | cRef =:= CgiRef ref =
  HtmlCRef (HtmlStruct "select" ((name ref):attrs) options) cRef
  where
    ref free
    options  = map (\(n,v) -> option ((value v):(mSel v)) [text n]) selMenu
    mSel val = if val==initSelVal then [selected] else []
-}

selectNoRef :: [HtmlAttr] -> [(String,String)] -> String -> HtmlExp
selectNoRef attrs selMenu initSelVal =
  sI selMenu initSel `addAttrs` attrs
 where
  initSel = maybe 0 Prelude.id (findIndex (==initSelVal) (map snd selMenu))

  sI :: [(String,String)] -> Int -> HtmlExp
  sI sellist sel = HtmlStruct "select" [] (selOption sellist sel)
   where
    selOption [] _ = []
    selOption ((n,v):nvs) i =
      HtmlStruct "option"
        ([("value",v)] ++ if i==0 then [("selected","selected")] else [])
        [htxt n] : selOption nvs (i-1)


small :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
small = HtmlStruct "small"

span :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
span = HtmlStruct "span"

strong :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
strong = HtmlStruct "strong"

textArea :: [HtmlAttr] -> CgiRef -> String -> HtmlExp
textArea attrs cRef cont =
  deleteAttr (deleteAttr (HTML.Base.textArea cRef (80,10) cont) "rows") "cols"
    `addAttrs` attrs
{- OLD:
  HtmlCRef (HtmlStruct "textarea" ((name ref):attrs) [text cont]) cRef
  where ref free
-}

textAreaNoRef :: [HtmlAttr] -> String -> HtmlExp
textAreaNoRef attrs cont =
  deleteAttr (deleteAttr (tA (80,10) cont) "rows") "cols"
    `addAttrs` attrs
 where
  tA (height,width) contents =
    HtmlStruct "textarea" [("rows",show height),("cols",show width)]
                          [htxt contents]

ul :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
ul = HtmlStruct "ul"

--------------------------------------------------------------------------------

-- Complex elements

buttonButton :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
buttonButton attrs = button $ ("type","button"):attrs

resetButton :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
resetButton attrs = button $ ("type","reset"):attrs

submitButton :: [HtmlAttr] -> HtmlHandler -> [HtmlExp] -> HtmlExp
submitButton attrs hdlr btnLabel
  | idOfCgiRef cref =:= ref -- instantiate cref argument
  = HtmlEvent
      (HtmlStruct "input" (("type","submit") : (name ref) : attrs) btnLabel)
                  cref hdlr
 where
  cref,ref free

formSubmitButton :: [HtmlAttr] -> String -> HtmlHandler -> HtmlExp
formSubmitButton attrs btnLabel hdlr =
  consAttrs attrs (HTML.Base.button btnLabel hdlr)

selectInput :: [HtmlAttr] -> CgiRef -> [(String,String)] -> Int -> HtmlExp
selectInput attrs cref sels initSel =
  selectionInitial cref sels initSel `addAttrs` attrs
{- OLD:
selectInput attrs cref sels initSel | cref =:= CgiRef ref =
  HtmlCRef (HtmlStruct "select" ((name ref):attrs) options) cref
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

consAttrs :: [(String,String)] -> HtmlExp -> HtmlExp
consAttrs _    (HtmlText txt)            = HtmlText txt
consAttrs nats (HtmlStruct tag [] hexps) = HtmlStruct tag nats hexps
consAttrs nats (HtmlStruct tag ((n,v):ats) hexps) 
  | n == "type" = HtmlStruct tag ((n,v):nats++ats) hexps -- skip type attribute
  | otherwise   = HtmlStruct tag (nats++(n,v):ats) hexps
consAttrs nats (HtmlCRef hexp cref)      = HtmlCRef  (consAttrs nats hexp) cref
consAttrs nats (HtmlEvent hexp cref hdlr) =
  HtmlEvent (consAttrs nats hexp) cref hdlr
consAttrs _    (HtmlAction act) = HtmlAction act

consAttr :: (String,String) -> HtmlExp -> HtmlExp
consAttr nat = consAttrs [nat]

consClass :: String -> HtmlExp -> HtmlExp
consClass cvalue = consAttr ("class",cvalue)

addToAttr :: HtmlExp -> (String,String) -> HtmlExp
addToAttr (HtmlText txt)            _      = HtmlText txt
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

setAttr :: HtmlExp -> (String,String) -> HtmlExp
setAttr (HtmlText txt)        _  = HtmlText txt
setAttr (HtmlStruct tag ats hexps) (n,v) =
  HtmlStruct tag (setAttr' ats) hexps
  where setAttr' [] = [(n,v)]
        setAttr' ((n',v'):ats') | n' == n   = ((n,v):ats')
                                   | otherwise = (n',v'):(setAttr' ats') 
setAttr (HtmlCRef hexp cref)       at = HtmlCRef  (setAttr hexp at) cref
setAttr (HtmlEvent hexp cref hdlr) at = HtmlEvent (setAttr hexp at) cref hdlr
setAttr (HtmlAction act)           _  = HtmlAction act

deleteAttr :: HtmlExp -> String -> HtmlExp
deleteAttr (HtmlText txt)        _  = HtmlText txt
deleteAttr (HtmlStruct tag ats hexps) n =
  HtmlStruct tag (deleteAttr' ats) hexps
  where deleteAttr' [] = []
        deleteAttr' ((n',v):ats') | n' == n   = ats'
                                  | otherwise = (n',v):(deleteAttr' ats') 
deleteAttr (HtmlCRef hexp cref)  at = HtmlCRef  (deleteAttr hexp at) cref
deleteAttr (HtmlEvent hexp cref hdlr) at =
  HtmlEvent (deleteAttr hexp at) cref hdlr
deleteAttr (HtmlAction act) _ = HtmlAction act


toValidAttrValue :: String -> String
toValidAttrValue = map (\c -> if c==' ' then '-' else toLower c)

stringToValidAttrValue :: String -> String
stringToValidAttrValue = map (\c -> if c==' ' then '-' else toLower c)

withClass :: ([HtmlExp] -> HtmlExp) -> String -> ([HtmlExp] -> HtmlExp)
withClass helem classVal = consClass classVal . helem

withAddClass :: ([HtmlExp] -> HtmlExp) -> String -> ([HtmlExp] -> HtmlExp)
withAddClass helem classVal hexps = helem hexps `addToClass` classVal

withAttrs
  :: ([HtmlExp] -> HtmlExp) -> [(String,String)] -> ([HtmlExp] -> HtmlExp)
withAttrs helem attrs hexps = helem hexps `addAttrs` attrs

withAttr 
  :: ([HtmlExp] -> HtmlExp) -> (String,String) -> ([HtmlExp] -> HtmlExp)
withAttr helem attr = withAttrs helem [attr]

withId :: ([HtmlExp] -> HtmlExp) -> String -> ([HtmlExp] -> HtmlExp)
withId helem idVal = withAttr helem ("id",idVal)

addId :: HtmlExp -> String -> HtmlExp
addId hExp idVal = addAttr hExp ("id",idVal)

--------------------------------------------------------------------------------