--------------------------------------------------------------------------------
--- This modules provides views for the module `AdminController` and primarily
--- exports pages with WUI forms for entity creation (languages and systems).
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version July 2020
--------------------------------------------------------------------------------

module View.Admin (
  languageCreationRendering, systemCreationRendering, editSystemRendering,
  listSystemView, listUserView,
  wLanguage, wSystem, wSystemType
) where

import Prelude hiding (div)
import Sort    ( sortBy )

import System.Controllers
import Model.ExecEnv
import System.SmapHtml
import System.SmapWui
import System.Views
import Model.Smap
--import SmapEntitiesToHtml

--------------------------------------------------------------------------------
-- Exported views                                                             --
--------------------------------------------------------------------------------

--- A rendering for a WUI form to create a new language.
languageCreationRendering :: HtmlExp -> (CgiEnv -> IO [HtmlExp]) -> View
languageCreationRendering =
  renderWui
    [h3 [] [addIcon, text " Add a new language to Smap"]]
    []
    "Add to Smap!"
    []
    []

--- A rendering for a WUI form to create a new system.
systemCreationRendering :: HtmlExp -> (CgiEnv -> IO [HtmlExp]) -> View
systemCreationRendering =
  renderWui
    [h3 [] [addIcon, text " Add a new system to Smap"]]
    []
    "Add to Smap!"
    []
    []

--- Supplies a WUI form to edit the given System entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editSystemRendering :: HtmlExp -> (CgiEnv -> IO [HtmlExp]) -> View
editSystemRendering =
  renderWui [text "Edit System"]  [] "Change!" [] []


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
               map listSystem (sortBy leqSystem systems))] []]
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

--- Supplies a list view for a given list of User entities.
listUserView :: [User] -> [HtmlExp]
listUserView users =
  [panelWith 10
     [text $ "Users in Smap (total: " ++ show (length users) ++ ")"]
     [spTable ([[[b [] [text "Name"]]
                ,[b [] [text "Email"]]
                ,[b [] [text "Is admin?"]]]] ++
               map listUser (sortBy leqUser users))] []]
  where
    listUser :: User -> [[HtmlExp]]
    listUser user =
         userToListView user ++ []
           --[[smBlueLinkBtn ("?users/edit/" ++ showUserKey user)
           --         [modifiedIcon, htxt " edit"]]
           --,[smBlueLinkBtn ("?users/delete/" ++ showUserKey user)
           --         [deleteIcon, htxt " delete"]]
           --]

    userToListView :: User -> [[HtmlExp]]
    userToListView user =
      [[text (userName user)]
      ,[text (userEmail user)]
      ,[text (if userIsAdmin user then "X" else " ")]]

--- Compares two User entities. This order is used in the list view.
leqUser :: User -> User -> Bool
leqUser x1 x2 = userName x1 <= userName x2

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
