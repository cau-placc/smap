--------------------------------------------------------------------------------
--- This modules provides views for the module `AdminController` and primarily
--- exports pages with WUI forms for entity creation (languages and systems).
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module AdminView (
  languageCreationForm,systemCreationForm,listSystemView,editSystemView
) where

import Prelude hiding (div)
import Sort(mergeSort)

import Controllers
import ExecEnvModel
import SmapHtml
import SmapWui
import Views
import Smap
--import SmapEntitiesToHtml

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

--- Compares two System entities. This order is used in the list view.
leqSystem :: System -> System -> Bool
leqSystem x1 x2 =
  (systemName x1,systemExecUrl x1) <= (systemName x2,systemExecUrl x2)

--- Supplies a list view for a given list of System entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of System entities.
listSystemView :: [System] -> [HtmlExp]
listSystemView systems =
  [panelWith 10
     [text  "Execution Systems in Smap"]
     [spTable ([[[b [] [text "Name"]],[b [] [text "Execution URL"]]]] ++
               map listSystem (mergeSort leqSystem systems))] []]
  where
    listSystem :: System -> [[HtmlExp]]
    listSystem system =
         systemToListView system ++
          ([[smBlueLinkBtn ("?systems/edit/" ++ showSystemKey system)
                    [modifiedIcon, htxt " edit"]]
           ,[smBlueLinkBtn ("?systems/delete/" ++ showSystemKey system)
                    [deleteIcon, htxt " delete"]]
           ])

    systemToListView :: System -> [[HtmlExp]]
    systemToListView system =
      [[text (systemName system)],[text (systemExecUrl system)]]

--- Supplies a WUI form to edit the given System entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editSystemView
 :: System -> Language -> [Language]
  -> (System -> Controller) -> [HtmlExp]
editSystemView system relatedLanguage possibleLanguages controller =
  renderWuiForm (wSystemType system relatedLanguage possibleLanguages)
                system controller [text "Edit System"]  [] [text "Change!"] [] []

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
    nameHelp    = ""
    execUrlHelp = ""
    nameErr     = "Please enter the system name."
    execUrlErr  = "Please enter the execution URL."

--- Transformation from data of a WUI form to entity type System.
tuple2System :: System -> (String,String,Language) -> System
tuple2System systemToUpdate (name ,execUrl ,language) =
  setSystemName
   (setSystemExecUrl
     (setSystemLanguageLangImplKey systemToUpdate (languageKey language))
     execUrl)
   name

--- Transformation from entity type System to a tuple
--- which can be used in WUI specifications.
system2Tuple :: Language -> System -> (String,String,Language)
system2Tuple language system =
  (systemName system,systemExecUrl system,language)

--- WUI Type for editing or creating System entities.
--- Includes fields for associated entities.
wSystemType :: System -> Language -> [Language] -> WuiSpec System
wSystemType system language languageList =
  transformWSpec (tuple2System system,system2Tuple language)
   (wSystem languageList)

--------------------------------------------------------------------------------
