--------------------------------------------------------------------------------
--- This modules provides views for the module `AdminController` and primarily
--- exports pages with WUI forms for entity creation (languages and systems).
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module AdminView (
  languageCreationForm,systemCreationForm
) where

import Prelude hiding (div)

import Controllers
import ExecEnvModel
import SmapHtml
import SmapWui
import Views

--------------------------------------------------------------------------------
-- Exported views                                                             --
--------------------------------------------------------------------------------

--- Supplies a WUI form to create a new language.
--- @param createLang - the controller that handles the language creation
languageCreationForm :: ((String,String,String) -> Controller) -> View
languageCreationForm createLang =
  renderWuiForm wLanguage ("","","") createLang 
    [h3 [] [addIcon,text " Add a new language to Smap"]]
    []
    [text "Add to Smap!"]
    []
    []

--- Supplies a WUI form to create a new system.
--- @param createSystem - the comntroller that handles the system creation
systemCreationForm :: [Language] -> ((String,String,Language) -> Controller) -> View
systemCreationForm langs createSystem =
  renderWuiForm (wSystem langs) ("","",head langs) createSystem
    [h3 [] [addIcon,text " Add a new system to Smap"]]
    []
    [text "Add to Smap!"]
    []
    []

--------------------------------------------------------------------------------
-- WUI components                                                             --
--------------------------------------------------------------------------------

-- The WUI specification for creating new languages.
wLanguage :: WuiSpec (String,String,String)
wLanguage = wSmapTriple
  (wSmapString 
    ("bookmark","text",True)
    "Enter the language name"
    "Language name"
    (nameHelp,nameErr)
    `withCondition` isRequired)
  (wSmapString 
    ("paperclip","text",False)
    "Enter the filename extension"
    "Filename extension"
    (fileExtHelp,fileExtErr)
    `withCondition` isRequired)
  (wSmapTextarea 
    ("file",(15,0),False)
    "Enter a source code template (optional)"
    "Source code template"
    (templateHelp,""))
  where
    nameHelp =
      ""
    fileExtHelp =
      ""
    templateHelp =
      ""
    nameErr =
      "Please enter a language name."
    fileExtErr =
      "Please enter a filename extension."

-- The WUI specification for creating new systems.
-- @param langs - the list of all languages in the database
wSystem :: [Language] ->WuiSpec (String,String,Language)
wSystem langs = wSmapTriple
  (wSmapString 
    ("bookmark","text",True)
    "Enter the system name"
    "System name"
    (nameHelp,nameErr)
    `withCondition` isRequired)
  (wSmapString
    ("link","text",False)
    "Enter the execution URL"
    "Execution URL"
    (execUrlHelp,execUrlErr)
    `withCondition` isRequired)
  (wSmapSelect "Choose the associated language" languageName langs)
  where
    nameHelp =
      ""
    execUrlHelp =
      ""
    nameErr =
      "Please enter the system name."
    execUrlErr =
      "Please enter the execution URL."

--------------------------------------------------------------------------------