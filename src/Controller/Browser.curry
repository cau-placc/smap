--------------------------------------------------------------------------------
--- This module provides controllers for functionalities of the Browser which
--- are primarily searching, listing and showing the programs on Smap. The
--- Browser also includes a dashboard (a kind of a Browser starting page where a
--- quick overview is given) and information about program tags. Furthermore,
--- the Browser supports user interaction like deleting programs, favoriting
--- programs and listing all created and favorited programs of the currently
--- authenticated user.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2020
--------------------------------------------------------------------------------

module Controller.Browser (
  browserController, modifyProgramForm, createCommentForm
) where

import Data.List ( split )

import HTML.Base ( HtmlFormDef, formDefWithID, formElem )
import HTML.Session

import Model.Comment
import Model.ExecEnv
import Model.Program
import Model.Tag
import Model.User
import Model.Smap(readTagKey)

import Controller.Comments
import Controller.Programs
import Controller.Users

import System.Alerts
import System.Authorization
import System.AuthorizedOperations
import System.Controllers
import System.Models
import System.Url
import System.Views

import View.Browser

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main browser controller which delegates tasks to other controllers in
--- this module depending on the URL path.
--- @param path - the current URL
browserController :: Url -> Controller
browserController url@(path,_) = case path of
  ["browser"]                 -> showDashboard
  ["browser","programs"]      -> showProgramList       url
  ["browser","myprograms"]    -> showUserProgramList   url
  ["browser","myfavorites"]   -> showUserFavoritesList url
  ["browser","tags"]          -> showTagList
  ["browser","deltag",tagkey] -> validateKeyAndApply (readTagKey tagkey)
                                   url doDeleteTag
  ["browser","visible",progKey] -> browseOn progKey "0" visibleProgramController
  ["browser","modprog",progKey] -> browseOn progKey "0" modifyProgramController
  ["browser","delprog",progKey] -> browseOn progKey "0" deleteProgramController
  ["browser","newcomm",progKey] -> browseOn progKey "0" createCommentController
  ["browser","addfav" ,progKey] -> browseOn progKey "0" addFavProgramController
  ["browser","remfav" ,progKey] -> browseOn progKey "0" remFavProgramController
  ["browser",progKey]           -> browseOn progKey "0" showProgramPage
  ["browser",progKey,versNum]   -> browseOn progKey versNum showProgramPage
  _                             -> showInvalidUrlErrorPage url
 where
  browseOn progKey versNum =
    validateKeyAndApply (readProgramKeyAndVersionNumber (progKey,versNum)) url

--------------------------------------------------------------------------------
-- Browser controllers                                                        --
--------------------------------------------------------------------------------

-- Returns a controller that displays the Browser dashboard. The dashboard gives
-- a quick overview about recent submissions and the most popular programs on
-- Smap. The dashboard also includes the search panel for advanced searches so
-- that users can use the dashboard as a starting point to browse through all
-- programs.
showDashboard :: Controller
showDashboard = 
  do recentProgs  <- getAllProgramsWith recentQuery
     popularProgs <- getAllProgramsWith popularQuery
     popularTags  <- getAllPopularTags
     langs        <- getAllLanguages 
     sortMenu     <- return $ map fst programSortingOptions
     return $ dashboard (langs,sortMenu,defaultSearchSettings)
                        popularTags
                        (take resultsPerPage recentProgs)
                        (take resultsPerPage popularProgs)
  where
    recentQuery  = defaultProgramQuery 
    popularQuery = withSorting leqProgramFavoriterCount defaultProgramQuery

-- Returns a controller that shows all program record entities (with possibly
-- specified search settings applied).
-- @param url - the URL (may contain search settings in the query string)
showProgramList :: Url -> Controller
showProgramList url = 
  checkAuthorization (browserOperation ShowAllPrograms) $ \authzData ->
  applySearchAndListPrograms url (getProgs authzData) programList
 where
  getProgs authzData =
    getAllProgramsWith . withIsVisibleOnly (not (isAdmin authzData))

-- Returns a controller that shows all programs (with possibly specified search
-- settings applied) created by the currently authenticated user. Displays an 
-- error page if the current user is not signed in.
-- @param url - the URL (may contain search settings in the query string)
showUserProgramList :: Url -> Controller
showUserProgramList url =
  checkAuthorization (browserOperation ShowUserPrograms) $ \authzData ->
  do mUserName <- return $ getUsernameFromAuthZData authzData
     maybe (showAccessDeniedErrorPage notSignedInErr)
           (\userName -> applySearchAndListPrograms url (getProgs userName)
                                                        userProgramList)
           mUserName
  where
    getProgs author = getAllProgramsWith . withIsVisibleOnly False
                                         . withExactAuthorName author
    notSignedInErr  = -- should be catched in the authorization check
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."

-- Returns a controller that shows all programs (with possibly specified search
-- settings applied) favorited by the currently authenticated user. Displays an
-- error page if the current user is not signed in.
-- @param url - the URL (may contain search settings in the query string)
showUserFavoritesList :: Url -> Controller
showUserFavoritesList url =
  checkAuthorization (browserOperation ShowUserFavorites) $ \authzData ->
  do mUserName <- return $ getUsernameFromAuthZData authzData
     maybe (showAccessDeniedErrorPage notSignedInErr)
           (\userName -> applySearchAndListPrograms url (getProgs userName)
                                                        userFavoritesList)
           mUserName
  where
    getProgs faver = getAllProgramsWith . withAddExactFavoriterName faver
    notSignedInErr = -- should be catched in the authorization check
      "You are not signed in. <a href=\"?signin\">Sign in</a> to proceed."

-- Gets a list of programs that satisfies the search settings from the URL query
-- string and applies a view on it.
-- @param url             - the current URL that contains the query string
-- @param getProgs        - a function that applies the search settings
-- @param programListView - a standard view for program lists
applySearchAndListPrograms  
  :: Url
  -> (ProgramQuery -> IO [Program])
  -> ([Program] -> [(Tag,Int)] -> Int -> SearchPanelData -> PagerData -> View)
  -> Controller
applySearchAndListPrograms url@(_,qStr) getProgs programListView = do
  (query,sets) <- getQueryAndSettingsFromQueryString qStr
  results      <- getProgs query
  totalResults <- return $ length results
  popularTags  <- getAllPopularTags
  langs        <- getAllLanguages
  sortMenu     <- return $ map fst programSortingOptions
  pagerData    <- getPagerData totalResults
  pageResults  <- getPageResults pagerData results
  return $ programListView pageResults popularTags totalResults
                           (langs,sortMenu,sets) pagerData
 where
  getPagerData totalResults = 
    do mPage <- getIntValueFromQueryString "page" qStr
       return (getCurrPage mPage,getTotalPages totalResults,url)
  getCurrPage = maybe 1 (\page -> if page <= 0 then 1 else page)
  getTotalPages totalResults =
    round $ (fromInt totalResults) / (fromInt resultsPerPage) + 0.49
  getPageResults (currPage,_,_) =
    return . take resultsPerPage . drop ((currPage-1)*resultsPerPage)

-- Returns a controller that simply displays a list of all tags on Smap sorted
-- by name in ascending order.
showTagList :: Controller
showTagList =
  checkAuthorization (browserOperation ShowAllTags) $ \azdata ->
  do (allTags,popularTags) <- getAllTagsAndPopularTags
     return $ tagList azdata allTags popularTags

-- Returns a controller to delete the tag given as an argument.
doDeleteTag :: TagKey -> Controller
doDeleteTag tagkey =
  getTagByKey tagkey >>=
  maybe (showStdErrorPage "No tag found with this key")
    (\tag -> checkAuthorization (browserOperation (DeleteTag tag)) $ \_ ->
       do prognums <- getTagNumber tag
          if prognums==0
           then deleteTag tag >>=
                either (\_ -> maybeSetAlert (successAlert tag) >> showTagList)
                       (showTransactionErrorPage tagDeletionFailedErr)
           else showStdErrorPage tagUndeletableErr)
  where
    successAlert tag = Just (SuccessAlert $ "Tag '"++tagName tag++"' deleted")
    tagDeletionFailedErr =
      "The tag deletion failed due to an unexpected internal error. See the "++
      "internal error message for additional details."
    tagUndeletableErr =
      "The tag cannot be deleted since it is now used in some program."

--- Controller that makes the program visible.
visibleProgramController :: (ProgramKey,Int) -> Controller
visibleProgramController (progKey,versNum) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ MakeVisible prog) $ \_ ->
           doUpdateProgramMetadata
             (const $ showProgramPage (progKey,versNum),
              Just programVisibleSucceededAlert)
             (setProgramIsVisible True prog))
 where
  programVisibleSucceededAlert =
    SuccessAlert $ "This program is now available for everyone on Smap."

--- Controller that modifiers the metadata of the program.
modifyProgramController :: (ProgramKey,Int) -> Controller
modifyProgramController (progKey,versNum) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ ModifyProgram prog) $ \_ -> do
             putSessionData browserStore (prog,versNum)
             return [formElem modifyProgramForm])

--- The data stored for executing the "modifyProgram" form.
browserStore :: SessionStore (Program,Int)
browserStore = sessionStore "browserStore"

modifyProgramForm :: HtmlFormDef (Program,Int)
modifyProgramForm =
  formDefWithID "Controller.Browser.modifyProgramForm" readData formHTML
 where
  readData = getSessionData browserStore failed

  formHTML (prog,versNum) = modifyProgramRendering prog modifyProgAndTags
   where
    modifyProgAndTags = doUpdateProgramMetadataWithTags
          (const $ showProgramPage (programKey prog, versNum),
           Just programEditingSucceededAlert)
    programEditingSucceededAlert =
      SuccessAlert $ "The program metadata has been modified."

--- Controller that modifiers the metadata of the program.
createCommentController :: (ProgramKey,Int) -> Controller
createCommentController (progKey,versNum) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ CreateComment) $ \_ -> do
             putSessionData browserStore (prog,versNum)
             return [formElem createCommentForm])

createCommentForm :: HtmlFormDef (Program,Int)
createCommentForm =
  formDefWithID "Controller.Browser.createCommentForm" readData formHTML
 where
  readData = getSessionData browserStore failed

  formHTML (prog,versNum) = createCommentRendering prog doCreateComCtrl
   where
    doCreateComCtrl
      = doCreateCommentForCurrentUser 
        (const $ showAccessDeniedErrorPage programNotFoundErr,Nothing)
        (const $ showProgramPage (programKey prog, versNum),Nothing)

--- Controller that deletes the program.
deleteProgramController :: (ProgramKey,Int) -> Controller
deleteProgramController (progKey,_) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ DeleteProgram prog) $ \_ ->
           doDeleteProgram
             (showDashboard, Just $ programDeletionSucceededAlert prog) prog)
 where
  programDeletionSucceededAlert prog =
    SuccessAlert $ "\"<code>"++programTitle prog++"</code>\" was "++
      "successfully deleted from Smap and is no longer available."

programNotFoundErr :: String
programNotFoundErr =
  "We couldn't find the program you were looking for. It might have been "++
  "deleted by its author or probably never existed."

--- Controller that adds the program to the favorites of
--- the currently authenticated user.
addFavProgramController :: (ProgramKey,Int) -> Controller
addFavProgramController (progKey,versNum) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ AddToFavorites prog) $ \_ ->
           doAddFavoritingForCurrentUser
             (const $ showAccessDeniedErrorPage programNotFoundErr,Nothing)
             (showProgramPage (progKey,versNum)                   ,Nothing)
             prog)

--- Controller that removes the program from the favorites
--- of the currently authenticated user.
remFavProgramController :: (ProgramKey,Int) -> Controller
remFavProgramController (progKey,versNum) = getProgramByKey progKey >>=
  maybe (showStdErrorPage programNotFoundErr)
        (\prog ->
           checkAuthorization (browserOperation $ RemoveFromFavorites prog) $
           \_ ->
           doRemoveFavoritingForCurrentUser
             (const $ showAccessDeniedErrorPage programNotFoundErr,Nothing)
             (showProgramPage (progKey,versNum)                   ,Nothing)
             prog)

------------------------------------------------------------------------------
-- Shows a specific version of an existing program in the Browser. In contrary
-- to the information shown in the IE, this view also includes the description
-- and information on associated entities like versions, tags and favoritings.
-- Furthermore, the Browser page of a program contains navigation elements to
-- add programs to the favorites or to delete programs (if authorized). Returns
-- an error pages if no program exists for the given key or if the version
-- number is not valid.
-- @param ctrlData - program key and version number
showProgramPage :: (ProgramKey,Int) -> Controller
showProgramPage (progKey,versNum) = do
  mProg <- getProgramByKey progKey
  maybe
    (showStdErrorPage programNotFoundErr)
    (\prog ->
        checkAuthorization (browserOperation $ ShowProgram prog) $ \azData -> do
          putSessionData browserStore (prog,versNum)
          maybe (showStdErrorPage $ versionNotFoundErr prog)
                (\validVersNum -> return $ programPage
                                             (prog,validVersNum)
                                             (formElem modifyProgramForm)
                                             (formElem createCommentForm)
                                             azData)
                (validVersionNumber (length $ programVersions prog)))
    mProg
 where
  validVersionNumber versCount =
    if versNum == 0 -- always points to latest version
      then Just versCount
      else if versNum < 0 || versNum > versCount
             then Nothing
             else Just versNum

  versionNotFoundErr prog =
    "We couldn't find the version you requested (see <a href=?browser/" ++
    showProgramKey prog ++ ">latest version</a> instead)."

--------------------------------------------------------------------------------
-- Handling query string search settings                                      --
--------------------------------------------------------------------------------

-- Parsing the query string

-- Extracts the search settings for programs from the query string and builds a
-- corresponding program query. Returns a pair containing the program query for
-- the model search and the search settings (for rendering the search
-- information in views) as an I/O action.
-- @param qStr - the query string containing the search settings
getQueryAndSettingsFromQueryString
  :: [(String,String)] -> IO (ProgramQuery,SearchSettings)
getQueryAndSettingsFromQueryString qStr =
  do mQ       <- getStrValueFromQueryString "q"       qStr
     mTargets <- getStrValueFromQueryString "targets" qStr
     mLang    <- getStrValueFromQueryString "lang"    qStr
     mSort    <- getStrValueFromQueryString "sort"    qStr
     mOrder   <- getStrValueFromQueryString "order"   qStr
     let targets   = maybe ["title","descr","tags"] (split (==',')) mTargets
         titleInTs = "title" `elem` targets
         descrInTs = "descr" `elem` targets
         tagsInTs  = "tags"  `elem` targets
     return ( maybe id withKeyword mQ
            $ (withInfixIgnoreCaseKeywordInTitle titleInTs)
            $ (withInfixIgnoreCaseKeywordInDescr descrInTs)
            $ (withExactIgnoreCaseKeywordInTags  tagsInTs)
            $ maybe id withExactIgnoreCaseImplLangName mLang
            $ (withSorting $ maybe defaultSort id $ getSorting mSort)
            $ withOrdering (getOrdering mOrder)
            $ defaultProgramQuery,
            (mQ,(titleInTs,descrInTs,tagsInTs),mLang,mSort,mOrder))
  where
    defaultSort = snd $ head $ programSortingOptions
    getSorting  = lookupSorting programSortingOptions . maybe "created" id
    getOrdering = maybe Descending (\order -> case order of
                                     "asc" -> Ascending
                                     _     -> Descending)

-- Sorting options

-- Type synonym for general sorting options. A sorting option is a pair
-- containing a pair of a value and a name (where the name also serves as the
-- identifier) for views and an actual sorting for the model query.
type SortingOption a = ((String,String),Sorting a)

-- Searches the given sorting options for the given sorting identifier and
-- returns the corresponding sorting. `Nothing` is returned if no such sorting
-- option exists.
-- @param sortingOptions - the list of sorting options
-- @param sortingId      - the identifier of the sorting
lookupSorting :: [SortingOption a] -> String -> Maybe (Sorting a)
lookupSorting [] _ = Nothing
lookupSorting (((_,name),sort):sortOpts) sortId
  | name == sortId = Just sort
  | otherwise      = lookupSorting sortOpts sortId

-- Sorting options for programs.
programSortingOptions :: [SortingOption Program]
programSortingOptions =
  [(("creation date (default)","created"   ),leqProgramFirstVersionDate )
  ,(("last modified date"     ,"modified"  ),leqProgramLatestVersionDate)
  ,(("popularity"             ,"popularity"),leqProgramFavoriterCount   )
  ,(("title"                  ,"title"     ),leqProgramTitle            )
  ,(("language"               ,"lang"      ),leqProgramImplLangName     )]

--------------------------------------------------------------------------------
-- Auxiliary constants                                                        --
--------------------------------------------------------------------------------

-- The amount of results per page in list views.
resultsPerPage :: Int
resultsPerPage = 10

--------------------------------------------------------------------------------