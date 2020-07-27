--------------------------------------------------------------------------------
--- This module primarily provides the general view of the interactive editor
--- (SmapIE) which covers all possible states of the component. In addition,
--- this module contains the views for program and version creation since these
--- operations can only be triggered from the IE (via the options menu, if
--- permitted) and therefore are considered to be part of its overall
--- functionality.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2020
--------------------------------------------------------------------------------

module View.SmapIE (
  smapIEPage, removeCRs, wProgram, wVersion
 ) where

import Char
import List
import Prelude hiding (div,span)

import Model.ExecEnv
import Model.Program
import System.Authorization
import System.AuthorizedOperations
import System.Controllers
import System.Execution
import System.SmapHtml
import System.SmapWui
import System.Views
import GenerateProgramURL
import System.Alerts

--------------------------------------------------------------------------------
-- HTML components for SmapIE views                                           --
--------------------------------------------------------------------------------

-- The standard rendering of the interactive editor (SmapIE) that covers all
-- possible states of the IE (blank, with initial editor value, between
-- executions, etc.).
-- @param mProg         - a possibly given existing program
-- @param execEnv       - the associated execution environment
-- @param mExecRes      - a possibly given execution result
-- @param initCode      - initial content of the editor
-- @param initSystemKey - key of the initially selected execution system
-- @param executeProg   - a controller for executing programs
-- @param tryShowPCForm - a controller that tries to show the program creation
--   form (which may fail if the source code is not executable) 
-- @param tryShowVCForm - the controller that tries to show the version creation
--   form (which may fail if the source code is not executable)
-- @param smapCtrl      - a controller for the SmapIE
-- @param authzData     - the current authorization data
smapIEPage
  :: Maybe Program -> ExecEnv -> Maybe ExecResult -> String -> String 
  -> ((ExecEnv,String,String,Maybe Program) -> Controller)
  -> ((ExecEnv,String,String) -> Controller)
  -> ((ExecEnv,String,String,Program) -> Controller)
  -> (Maybe Program -> ExecEnv -> Maybe ExecResult -> String -> String
                    -> Controller)
  -> AuthZData -> [HtmlExp]
smapIEPage   mProg 
             execEnv@(lang,systems)
             mExecRes
             initCode 
             initSystemKey 
             executeProg
             tryShowPCForm
             tryShowVCForm
             smapCtrl
             authzData =
  [(container `withId` "smap-ie")
    [row
      [panelDefault
        [panelBody
          -- editor
          [col [Xs 7]
            [div [classA "clearfix"]
              [div [classA "pull-left"]
                [label []
                  [codeIcon,text " Source code"]]
              ,div [classA "pull-right"]
                [span [classA "text-muted"] [text "Language: "]
                ,labelDefault [text langName]]]
            ,textArea [] codeRef initCode
            `addId` "editor"
            ,script [] [text codeMirrorInstance]]
          -- execution result, button bar and program information
          ,col [Xs 5]
            [row
              [div [classA $ "form-group"++execResType]
                [div [classA "clearfix"]
                  [div [classA "pull-left"]
                    [label [] [executionIcon,text " Execution result"]]
                  ,div [classA "pull-right"]
                    [span [classA "help-block"] execResInfo]]
                ,textArea [classA "form-control",readonly] _ res
                `addId` "execution-result"
                ,select [classA "form-control input-sm"]
                  systemRef systemMenu initSystemKey]]
            ,row
              [(div [classA "clearfix"] `withId` "button-bar")
                [div [classA "pull-left"]
                  [formSubmitButton 
                    [classA "btn btn-success",title runButtonTooltip]
                    "Run" execHdlr
                    --[executionIcon,text " Run"]
                  `addId` "run-button"
                  ,div [classA "btn-group"]
                    [buttonButton
                      [classA "btn btn-warning",title resetButtonTooltip]
                      [text " Reset"]
                    `addId` "reset-button"
                    ,buttonButton
                      [classA "btn btn-danger",title clearButtonTooltip]
                      [text " Clear"]
                    `addId` "clear-button"]]
                ,div [classA "btn-group pull-right"]
                  [buttonButton 
                    [classA "btn btn-default dropdown-toggle",dropdownToggle]
                    [optionsIcon,text " Options ",caret]
                  ,optionsMenu]]
              ,(div [classA "well"] `withId` "program-info")
                programInfo
              ,(div [classA "text-muted small"] `withId` "program-info-label")
                [infoIcon,text " Program information"]]]]]]]]
  where
    codeRef,systemRef free
    langName        = languageName $ lang
    systemMenu      = map (\s -> (systemName s,showSystemKey s)) systems
    execHdlr e      = next $ executeProg (execEnv,e systemRef,getCode e,mProg)
    downloadHdlr e  = return (HtmlAnswer "text/plain" (removeCRs (getCode e)))
    uploadUrlHdlr e = do setAlert (InfoAlert $ uploadLinkText (getCode e))
                         next $ smapCtrl mProg execEnv mExecRes (getCode e)
                                         initSystemKey
    pcFormHdlr e    = next $ tryShowPCForm (execEnv,e systemRef,getCode e)
    vcFormHdlr p e  = next $ tryShowVCForm (execEnv,e systemRef,getCode e,p)
    getCode e       = removeCRs $ e codeRef
    (execResType,execResInfo,res) =
      maybe 
      ("",[text "No execution result yet."],"")
      (\execRes -> case execRes of
        ExecSuccess out -> 
          (" has-success",[execSuccessIcon,text " Execution successful."],out)
        ExecError   out -> 
          (" has-error"  ,[execErrorIcon  ,text " Execution failed."]    ,out))
      mExecRes
    optionsMenu =
      ul [classA "dropdown-menu pull-right",role "menu"] $
        maybe noProgOpts progOpts mProg
      ++[li [classA "divider"] []
        ,li []
          [--downloadIcon,
           formSubmitButton [classA "btn btn-link",("formtarget","_blank")]
             "Download source code" downloadHdlr]
        ,li []
          [--uploadIcon,
           formSubmitButton [classA "btn btn-link"]
             "Generate upload URL" uploadUrlHdlr]]
    noProgOpts =
      [li []
        [a [href $ newProgBaseUrl++"/"++(map toLower langName)]
          [reloadIcon,text $ " Reload "++langName++" template"]]]++
      byAuthorization (smapIEOperation CreateProgram authzData)
        [li [classA "divider"] []
        ,li [] 
          [linkSubmitBtn "Save program to Smap" pcFormHdlr]]
        (\_ -> [empty])
    progOpts prog =
      [li []
        [a [href $ newProgBaseUrl++"/"++(map toLower langName)]
          [smapIEIcon,text $ " New "++langName++" program"]]
      ,li []
        [a [href $ openProgramBaseUrl++(showProgramKey prog)]
          [reloadIcon,text $ " Reload program "++(showProgramKey prog)]]
      ,li []
        [a [href $ progInBrowserBaseUrl++"/"++(showProgramKey prog)]
          [browserIcon,text " View in Browser"]]]++
      byAuthorization (smapIEOperation (CreateVersion prog) authzData)
        [li [classA "divider"] []
        ,li [] 
          [linkSubmitBtn "Save as new version" (vcFormHdlr prog)]]
        (\_ -> [empty])
    codeMirrorInstance =
      "var code = CodeMirror.fromTextArea(document.getElementById('editor'), "++
      "{\n"++
      "  autoCloseBrackets: true,\n"++
      "  extraKeys: {\n"++
      "    Tab: function() {\n"++
      "      code.replaceSelection('  ','end');\n"++
      "    }\n"++
      "  },\n"++
      "  lineNumbers: true,\n"++
      "  lineWrapping: true,\n"++
      "  matchBrackets: true,\n"++
      "  mode: '"++map toLower langName++"',\n"++
      "  styleActiveLine: true,\n"++
      "  tabSize: 2,\n"++
      "  theme: 'smap'\n"++
      "});\n"
    runButtonTooltip =
      "Run program!"
    resetButtonTooltip =
      "Reset the source code editor to its inital value on page load."
    clearButtonTooltip =
      "Empty the source code editor completely."
    programInfo =
      [infoLine [idIcon      ,text " Program ID:"] progId
      ,infoLine [titleIcon   ,text " Title:"     ] progTitle
      ,infoLine [languageIcon,text " Language:"  ] langName
      ,infoLine [userIcon    ,text " Author:"    ] author
      ,infoLine [versionIcon ,text " Version:"   ] versNum]
    progId    = maybe "-" (\p -> showProgramKey p) mProg
    progTitle = maybe "-" programTitle mProg
    author    = maybe "-" (userName . programAuthor) mProg
    versNum   = maybe "-" (show . versionNumber . programLatestVersion) mProg
    infoLine name value =
      div [classA "clearfix"]
        [div [classA "pull-left" ] name
        ,div [classA "pull-right"] [span [classA "text-muted"] [text value]]]

    --- Create text for upload link:
    uploadLinkText progcode =
     "Remember <strong><a href=\""++generateUploadURL langName progcode++"\">"++
     "this link</a></strong> for uploading your program to Smap "++
     "in some future time."

--------------------------------------------------------------------------------
-- WUI components                                                             --
--------------------------------------------------------------------------------

-- The WUI specification for creating new programs. Previously specified
-- attributes and associated entities are hidden or displayed as constant
-- values.
wProgram :: WuiSpec (String,String,Bool,Language,User,String,String)
wProgram = wSmap7Tuple
  (wSmapString
    ("bookmark","text",True) "Enter a title" "Title"
    (titleHelp,titleErr)
  `withCondition` isRequiredWithAtMost50Characters)
  (wSmapTextarea 
    ("align-left",(4,0),False) "Enter a description" "Description"
    (descrHelp,""))
  (wSmapSelectBool
    "Should this program be available for everyone?"
    "Yes, this program should be accessible for everyone."
    "No, only I want to be able to access this program.")
  (wSmapConstant
    [text "Language:"] (\lang -> 
    [labelDefault [text $ languageName lang]]))
  (wSmapConstant 
    [text "Author:"] (\user -> 
    [span [classA "text-muted"] [userIcon,text $ ' ':userName user]]))
  wHidden
  (wSmapTextarea
    ("tags",(4,0),False) "Enter a list of tags (separated by spaces)"
                         "Tag1 Tag2 Tag3 ..."
    (tagsHelp,tagsErr)
  `withCondition` atLeast3TagsInList)
  where
    isRequiredWithAtMost50Characters input =
      (not $ null input) && (length input <= 50)
    atLeast3TagsInList =
      (3<=) . length . nub . filter (not . null) . split isSpace 
    isSpace c = c==' ' || c=='\n' || c=='\r' || c=='\t'
    titleHelp =
      "The title is part of the information shown when listing and searching "++
      "programs in the Browser. Give a concise (at most 50 characters) but me"++
      "aningful description to entitle your submission!"
    descrHelp =
      "The description is part of the information shown when previewing progr"++
      "ams in the Browser."
    tagsHelp =
      "Tags are used to categorize programs and make them easier to find. Ent"++
      "er at least 3 different tags as a list of space-separated strings (a c"++
      "omplete list of all tags can be found <a href=\"?browser/tags\" target"++
      "=\"_blank\">here <span class=\"glyphicon glyphicon-new-window\"></span"++
      "></a>)."
    titleErr =
      "Please enter a title with at most 50 characters."
    tagsErr =
      "Please enter at least 3 different tags to classify your program."

-- The WUI specification for creating new versions. Previously specified
-- attributes and associated entities are hidden or displayed as constant
-- values.
wVersion :: WuiSpec (Int,String,String,Program)
wVersion = wSmap4Tuple
  (wSmapConstant
    [text "Version number:"] (\number ->
    [span [classA "text-muted"] [text $ show number]]))
  wHidden
  (wSmapTextarea
    ("align-left",(6,0),True) "Enter a version message" "Version message"
    (messageHelp,messageErr)
  `withCondition` isRequired)
  (wSmapConstant
    [text "Program ID:"] (\prog ->
    [span [classA "text-muted"] [text $ showProgramKey prog]]))
  where
    messageHelp =
      "The version message should describe the changes as compared to the pre"++
      "vious version of this program."
    messageErr =
      "Please enter a version message."

--------------------------------------------------------------------------------
-- Auxiliary functions                                                        --
--------------------------------------------------------------------------------

-- Replaces all `\r\n` escape sequences by `\n` to avoid HTML representation
-- bugs.
-- @param input - the string input value (usually from a textarea)
removeCRs :: String -> String
removeCRs []         = []
removeCRs [c]        = [c]
removeCRs (c1:c2:cs) = if c1=='\r'&&c2=='\n' 
                       then '\n':removeCRs cs
                       else c1  :removeCRs (c2:cs)

--------------------------------------------------------------------------------