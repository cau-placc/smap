--------------------------------------------------------------------------------
--- This module provides views for all kinds of static pages (like the landing
--- page, the help page or the about page).
---
--- @author Lasse Kristopher Meyer
--- @version July 2020
--------------------------------------------------------------------------------

module View.Static (
  landingPage,helpPage,aboutPage
) where

import Prelude hiding (div,span)

import Control.SetFunctions
import System.Views
import System.SmapHtml

--------------------------------------------------------------------------------
-- Exported views for static pages                                            --
--------------------------------------------------------------------------------

--- The landing page view.
landingPage :: View
landingPage = 
  [jumbotron
    [container
      [h1 []  [b [] [text "Welcome to Smap!"]]
      ,p [] [text headerText]]]
  ,(container `withId` "landing-page")
    [row
      [col [Md 4]
        [img [src "images/smap-ie.png",classA "img-circle"]
        ,h3 [] [glyphicon "edit",text " SmapIE"]
        ,text smapIEInfo]
      ,col [Md 4]
        [img [src "images/browser.png",classA "img-circle"]
        ,h3 [] [glyphicon "list",text " Browser"]
        ,text browserInfo]
      ,col [Md 4]
        [img [src "images/users.png",classA "img-circle"]
        ,h3 [] [userIcon,text " Users"]
        ,text usersInfo]]
    ,(row `withId` "help-line")
      [helpIcon,text $ ' ':helpText]]]
  where 
    headerText =
      "Smap is primarily an interactive source code editor that lets you writ"++
      "e programs and execute them online in various programming languages. I"++
      "n addition, Smap allows you to save and manage your creations and make"++
      " them available to other users."
    smapIEInfo =
      "Write, execute and save programs and code snippets using SmapIE - an i"++
      "nteractive source code editor that lets you run your code while develo"++
      "ping it."
    browserInfo =
      "Use the Browser to search and preview submitted programs for specific "++
      "programming tasks and try out the creations from other users by loadin"++
      "g them to SmapIE."
    usersInfo =
      "Sign in and add your own programs to Smap or save your mostly used pro"++
      "grams from other users to your list of favorites for quick access."
    helpText =
      "Don't know how to start? View the <a href=\"?help\">help page</a> for "++
      "more information."

--- The help page view.
helpPage :: [(String,String,String)] -> View
helpPage helpTexts =
  [(container `withId` "help-page")
    [row
      [div [classA "col-xs-8 col-xs-offset-2"]
        [panelDefault
          [panelBody
            [pageHeader [h3 [] [helpIcon,text " Help"]]
            ,div [classA "info"]
              [text infoText]
            ,(div [classA "panel-group"] `withId` "accordion")
              (map helpPanel helpTexts)
            ]
          ,panelFooter
            []]]]]]
  where
    helpPanel (helpPanelId,helpPanelTitle,helpPanelContent) =
      panelDefault
        [panelHeading
          [h4 [classA "panel-title"]
            [helpIcon,text " "
            ,a [collapseToggle,parentId "accordion",href $ '#':helpPanelId]
              [text helpPanelTitle]]]
        ,(div [classA "panel-collapse collapse"] `withId` helpPanelId)
          [panelBody
            [text (replaceLinks helpLink helpPanelContent)]]]
    helpLink helpPanelKeyword helpPanelId =
      "<a data-toggle=\"collapse\" data-parent=\"accordion\" href=\"#"++
      helpPanelId++"\"><span class=\"glyphicon glyphicon-question-sign\"></sp"++
      "an><b>"++helpPanelKeyword++"</b></a>"
    infoText =
      "<strong>Welcome to the help page!</strong> Below is a selection of com"++
      "mon questions with answers that may help you if you are new to Smap. S"++
      "ee the answer to a question by clicking on the particular title."

-- Replaces in a text occurrences of "[name](ref)" by (repfun name ref),
-- where repfun is the first argument.
replaceLinks :: (String -> String -> String) -> String -> String
replaceLinks repfun s =
  if isEmpty reps then s
                  else replaceLinks repfun (selectValue reps)
 where
  reps = set2 replaceLink repfun s

replaceLink :: (String -> String -> String) -> String -> String
replaceLink repfun (pre ++ "[" ++ name ++ "](" ++ ref ++ ")" ++ suf) =
  pre ++ repfun name ref ++ suf

--- The about page view.
aboutPage :: View
aboutPage =
  [(container `withId` "about-page")
    [row
      [col [Xs 8,XsOffset 2]
        [panelDefault
          [panelBody
            [pageHeader [h3 [] [aboutIcon,text " About"]]
            ,h4 [] [glyphicon "user",text " Initially developed by"]
            ,span [classA "text-muted"] 
              [text "Lasse Kristopher Meyer "
            ,a [href $ "mailto:"++mail] [glyphicon "envelope"]]
            ,br []
            ,span [classA "text-muted small"] 
              [text "(as part of a bachelor's project in 2013/2014)"]
            ,hr []
            ,h4 [] [glyphicon "user",text " Maintained by"]
            ,a [href "http://www.informatik.uni-kiel.de/~mh/",targetBlank]
               [text "Michael Hanus"]
            ,hr []
            ,h4 [] [glyphicon "link",text " Further links"]
            ,ul [] 
              [li [] [a [href cauUrl] [text cauTxt]]
              ,li [] [a [href ifiUrl] [text ifiTxt]]
              ,li [] [a [href psUrl ] [text psTxt ]]]
            ,hr []
            ,h4 [] [glyphicon "fire",text " Powered by"]
            ,row
              [col [Xs 2]
                [a [href curryUrl,classA "thumbnail",targetBlank]
                  [img [src "images/curry.png",alt "Curry"]]]
              ,col [Xs 2]
                [a [href spiceyUrl,classA "thumbnail",targetBlank]
                  [img [src "images/spicey.png",alt "Spicey"]]]
              ,col [Xs 2]
                [a [href codemirrorUrl,classA "thumbnail",targetBlank]
                  [img [src "images/codemirror.png",alt "CodeMirror"]]]
              ,col [Xs 2]
                [a [href bootstrapUrl,classA "thumbnail",targetBlank]
                  [img [src "images/bootstrap3.png",alt "Bootstrap 3"]]]
              ,col [Xs 2]
                [a [href glyphiconsUrl,classA "thumbnail",targetBlank]
                  [img [src "images/glyphicons.png",alt "GLYPHICONS"]]]]]
          ,panelFooter 
            []]]]]]
  where
    mail =
      "lkm@informatik.uni-kiel.de"
    cauUrl =
      "http://www.uni-kiel.de/index-e.shtml"
    ifiUrl =
      "http://www.inf.uni-kiel.de/en"
    psUrl =
      "https://www.informatik.uni-kiel.de/en/programming-languages-and-compil"++
      "er-construction/"
    cauTxt =
      "University of Kiel"
    ifiTxt =
      "Department of Computer Science"
    psTxt =
      "Research group of Programming Languages and Compiler Construction"
    curryUrl =
      "http://www-ps.informatik.uni-kiel.de/currywiki/"
    spiceyUrl = 
      "http://www.informatik.uni-kiel.de/~pakcs/spicey/"
    codemirrorUrl =
      "http://codemirror.net/"
    bootstrapUrl =
      "http://getbootstrap.com/"
    glyphiconsUrl = 
      "http://glyphicons.com/"

--------------------------------------------------------------------------------