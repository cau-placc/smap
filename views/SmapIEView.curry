--------------------------------------------------------------------------------
--- This module primarily provides the general view of the interactive editor
--- (SmapIE) which covers all possible states of the component. In addition,
--- this module contains the views for program and version creation since these
--- operations can only be triggered from the IE (via the options menu, if
--- permitted) and therefore are considered to be part of its overall
--- functionality.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2014
--------------------------------------------------------------------------------

module SmapIEView (
  smapIE,programCreationForm,versionCreationForm, removeCRs
) where

import Char
import List
import Prelude hiding (div,span)

import Authorization
import AuthorizedOperations
import Controllers
import Execution
import ExecEnvModel
import ProgramModel
import SmapHtml
import SmapWui
import Views
import GenerateProgramURL

--------------------------------------------------------------------------------
-- Exported SmapIE views                                                      --
--------------------------------------------------------------------------------

--- The general view of the interactive editor (SmapIE)!
--- @param mProg         - a possibly given existing program
--- @param execEnv       - the associated execution environment
--- @param mExecRes      - a possibly given execution result
--- @param initCode      - initial content of the editor
--- @param initSystemKey - key of the initially selected execution system
--- @param executeProg   - a controller for executing programs
--- @param tryShowPCForm - a controller that tries to show the program creation
---   form (which may fail if the source code is not executable) 
--- @param tryShowVCForm - a controller that tries to show the version creation
---   form (which may fail if the source code is not executable)
--- @param authzData     - the current authorization data
smapIE
  :: Maybe Program -> ExecEnv -> Maybe ExecResult -> String -> String 
  -> ((ExecEnv,String,String,Maybe Program) -> Controller) 
  -> ((ExecEnv,String,String) -> Controller) -> ((ExecEnv,String,String,Program)
  -> Controller) -> AuthZData -> View
smapIE = renderSmapIE

--- Supplies a WUI form to create a new program. The form expects additional
--- source code metadata and a list of tags to classify the program.
--- @param lang         - the implementation language
--- @param user         - the author
--- @param code         - the program source code
--- @param createProg   - the controller that performs the program creation
--- @param backToSmapIE - a controller to return to the IE without losing data
programCreationForm 
  :: Language -> User -> String
  -> ((String,String,Bool,Language,User,String,String) -> Controller)
  -> Controller -> View
programCreationForm lang user code createProg backToSmapIE =
  renderWuiForm wProgram ("","",True,lang,user,code,"") createProg
    [h3 [] [saveIcon,text " Save program to Smap"]]
    [text info]
    [text "Save to Smap!"]
    [orangeSubmitBtn backHdlr [smapIEIcon,text " Go back &raquo;"]]
    []
  where
    backHdlr _ = next $ backToSmapIE
    info =
      "Add some metadata information to your program to complete the saving p"++
      "rocess!"

--- Supplies a WUI form to create a new version. The form expects a version
--- message that specifies the changes to the previous version.
--- @param number       - the version number
--- @param code         - the version source code
--- @param prog         - the corresponding program
--- @param createVers   - the controller that performs the version creation
--- @param backToSmapIE - a controller to return to the IE without losing data
versionCreationForm 
  :: Int -> String -> Program -> ((Int,String,String,Program) -> Controller) 
  -> Controller -> View
versionCreationForm number code prog createVers backToSmapIE =
  renderWuiForm wVersion (number,code,"",prog) createVers
    [h3 [] [saveIcon,text " Save as new version"]]
    [text info]
    [text "Save to Smap!"]
    [orangeSubmitBtn backHdlr [smapIEIcon,text " Go back &raquo;"]]
    []
  where
    backHdlr _ = next $ backToSmapIE
    info =
      "Just add a version message to complete the saving process!"

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
-- @param authzData     - the current authorization data
renderSmapIE
  :: Maybe Program -> ExecEnv -> Maybe ExecResult -> String -> String 
  -> ((ExecEnv,String,String,Maybe Program) -> Controller)
  -> ((ExecEnv,String,String) -> Controller) -> ((ExecEnv,String,String,Program)
  -> Controller) -> AuthZData -> [HtmlExp]
renderSmapIE mProg 
             execEnv@(lang,systems)
             mExecRes
             initCode 
             initSystemKey 
             executeProg
             tryShowPCForm
             tryShowVCForm
             authzData =
  [input [("type","hidden"),value "smap-ie"] -- for styling purposes
  ,(container `withId` "smap-ie")
    [row
      [panelDefault
        [panelBody
          -- editor
          [col [Xs 7]
            [div [class "clearfix"]
              [div [class "pull-left"]
                [label []
                  [codeIcon,text " Source code"]]
              ,div [class "pull-right"]
                [span [class "text-muted"] [text "Language: "]
                ,labelDefault [text langName]]]
            ,textarea [] codeRef initCode
            `addId` "editor"
            ,script [] [text codeMirrorInstance]]
          -- execution result, button bar and program information
          ,col [Xs 5]
            [row
              [div [class $ "form-group"++execResType]
                [div [class "clearfix"]
                  [div [class "pull-left"]
                    [label [] [executionIcon,text " Execution result"]]
                  ,div [class "pull-right"]
                    [span [class "help-block"] execResInfo]]
                ,textarea [class "form-control",readonly] _ res
                `addId` "execution-result"
                ,select [class "form-control input-sm"]
                  systemRef systemMenu initSystemKey]]
            ,row
              [(div [class "clearfix"] `withId` "button-bar")
                [div [class "pull-left"]
                  [submitButton 
                    [class "btn btn-success",title runButtonTooltip] execHdlr
                    [executionIcon,text " Run"]
                  `addId` "run-button"
                  ,div [class "btn-group"]
                    [buttonButton
                      [class "btn btn-warning",title resetButtonTooltip]
                      [text " Reset"]
                    `addId` "reset-button"
                    ,buttonButton
                      [class "btn btn-danger",title clearButtonTooltip]
                      [text " Clear"]
                    `addId` "clear-button"]]
                ,div [class "btn-group pull-right"]
                  [buttonButton 
                    [class "btn btn-default dropdown-toggle",dropdownToggle]
                    [optionsIcon,text " Options ",caret]
                  ,optionsMenu]]
              ,(div [class "well"] `withId` "program-info")
                programInfo
              ,(div [class "text-muted small"] `withId` "program-info-label")
                [infoIcon,text " Program information"]]]]]]]]
  where
    codeRef,systemRef free
    langName        = languageName $ lang
    systemMenu      = map (\s -> (systemName s,showSystemKey s)) systems
    execHdlr e      = next $ executeProg   (execEnv,e systemRef,getCode e,mProg)
    downloadHdlr e  = return (HtmlAnswer "text/plain" (removeCRs (getCode e)))
    uploadUrlHdlr e = return (uploadLinkPage
                               (generateUploadURL langName (getCode e)))
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
      ul [class "dropdown-menu pull-right",role "menu"] $
        maybe noProgOpts progOpts mProg
      ++[li [class "divider"] []
        ,li []
          [submitButton [class "btn btn-link",("formtarget","_blank")]
             downloadHdlr [downloadIcon,text " Download source code"]]
        ,li []
          [submitButton [class "btn btn-link",("formtarget","_blank")]
             uploadUrlHdlr [uploadIcon,text " Generate upload URL"]]]
    noProgOpts =
      [li []
        [a [href $ newProgBaseUrl++"/"++(map toLower langName)]
          [reloadIcon,text $ " Reload "++langName++" template"]]]++
      byAuthorization (smapIEOperation CreateProgram authzData)
        [li [class "divider"] []
        ,li [] 
          [linkSubmitBtn pcFormHdlr [saveIcon,text " Save program to Smap"]]]
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
        [li [class "divider"] []
        ,li [] 
          [linkSubmitBtn (vcFormHdlr prog)
            [saveIcon,text " Save as new version"]]]
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
      div [class "clearfix"]
        [div [class "pull-left" ] name
        ,div [class "pull-right"] [span [class "text-muted"] [text value]]]

--- Create form to show upload link:
uploadLinkPage uplnk =
  HtmlForm "Upload Link" []
   [text "Remember ",
    a [href uplnk] [text "this link"],
    text " for uploading your program to Smap in some future time."]

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
    [span [class "text-muted"] [userIcon,text $ ' ':userName user]]))
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
    [span [class "text-muted"] [text $ show number]]))
  wHidden
  (wSmapTextarea
    ("align-left",(6,0),True) "Enter a version message" "Version message"
    (messageHelp,messageErr)
  `withCondition` isRequired)
  (wSmapConstant
    [text "Program ID:"] (\prog ->
    [span [class "text-muted"] [text $ showProgramKey prog]]))
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