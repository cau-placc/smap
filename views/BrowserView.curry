--------------------------------------------------------------------------------
--- This module provides views for the Browser component of Smap which includes
--- the dashboard, list views for programs and tags and a web page with detailed
--- information for each program.
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module BrowserView (
  dashboard,programList,userProgramList,userFavoritesList,programPage,tagList,
  SearchSettings,SearchPanelData,PagerData,defaultSearchSettings
) where

import Char
import List
import Prelude hiding (div,span)
import Time

import Authorization
import AuthorizedOperations
import Controllers
import ProgramModel
import Url
import UserModel
import Views
import SmapHtml

--------------------------------------------------------------------------------
-- Exported Browser views                                                     --
--------------------------------------------------------------------------------

--- The Browser dashboard view. The dashboard gives a quick overview about
--- recent submissions and the most favorited programs on Smap. It serves as a
--- kind of starting page for the Browser. Thus, it includes the panel for
--- advanced searches in the sidebar.
--- @param searchPanelData - data for rendering the search panel
--- @param popularTags     - a list of all tags sorted by popularity (sidebar)
--- @param recentProgs     - a list of the most recent submissions
--- @param popularProgs    - a list of the most favorited submissions
dashboard :: SearchPanelData -> [Tag] -> [Program] -> [Program] -> View
dashboard searchPanelData@(langs,_,_) popularTags recentProgs popularProgs =
  renderBrowser
    (renderStdSidebar searchPanelData popularTags allProgramsBaseUrl)
    [h3 [] [dashboardIcon,text " Dashboard"]]
    [text headerText]
    [(col [Xs 6] `withAddClass` "border-right") $
      [pageHeader
        [h4 [class "left"] 
          [recentIcon,text " Recently added ",small [] [text "programs"]]
        ,a [href allProgramsBaseUrl,class "right"] 
          [text "see all &raquo;"]]
      ,renderLanguageNameList Nothing langNamesLowered
      ,span [class "small"] [text " /"]
      ,br []
      ,renderProgramList noProgsMsg recentProgs False]
    ,col [Xs 6]
      [pageHeader
        [h4 [class "left"] 
          [favoriteIcon,text " Most favorited ",small [] [text "programs"]]
        ,a [href $ allProgramsBaseUrl++"?sort=popularity",class "right"] 
          [text "see all &raquo;"]]
      ,renderLanguageNameList (Just "sort=popularity") langNamesLowered
      ,span [class "small"] [text " /"]
      ,br []
      ,renderProgramList noProgsMsg popularProgs False]]
    []   
  where
    langNamesLowered = map (map toLower . languageName) langs
    renderLanguageNameList addQuery = 
      span [class "small"] . (concatMap $ \langNameLowered ->
      let urlSuffix = maybe "" ('&':) addQuery
       in [text " / "
          ,a [href $ allProgramsBaseUrl++"?lang="++langNameLowered++urlSuffix]
            [text langNameLowered]])
    headerText = 
      "<strong>Welcome to the dashboard!</strong> The dashboard gives you a q"++
      "uick overview about recent submissions and the most popular programs o"++
      "n Smap. If you want to search programs with specific keywords or attri"++
      "butes, use the quick search in the navigation bar or the form for adva"++
      "nced searches on the left."
    noProgsMsg = "Sorry, there is nothing to show yet."

--- List view for the results of general program searches.
--- @param results - the program list
--- @param tags    - top 25 popular tags
--- @param total   - total amount of results
--- @param spData  - data for rendering the search panel
--- @param pData   - data for rendering the pager
programList
  :: [Program] -> [Tag] -> Int -> SearchPanelData -> PagerData -> View
programList =
  renderProgramListView header breadcrumb
  where
    breadcrumb = li [class "active"] [text "Programs"]
    header     = [browserIcon,text " All programs "]

--- List view for the results of a search in the programs of the currently
--- authenticated user.
--- @param results - the program list
--- @param tags    - top 25 popular tags
--- @param total   - total amount of results
--- @param spData  - data for rendering the search panel
--- @param pData   - data for rendering the pager
userProgramList
  :: [Program] -> [Tag] -> Int -> SearchPanelData -> PagerData -> View
userProgramList =
  renderProgramListView header breadcrumb
  where
    breadcrumb = li [class "active"] [text "My programs"]
    header     = [userProgramsIcon,text " Your programs "]

--- List view for the results of a search in the favorites of the currently
--- authenticated user.
--- @param results - the program list
--- @param tags    - top 25 popular tags
--- @param total   - total amount of results
--- @param spData  - data for rendering the search panel
--- @param pData   - data for rendering the pager
userFavoritesList
  :: [Program] -> [Tag] -> Int -> SearchPanelData -> PagerData -> View
userFavoritesList =
  renderProgramListView header breadcrumb
  where
    breadcrumb = li [class "active"] [text "My favorites"]
    header     = [userFavoritesIcon,text " Your favorites "]

-- General rendering of the list view for programs. Renders results from program
-- searches in general.
-- @param header          - the header text
-- @param breadcumb       - breadcrumb navigation element
-- @param results         - the program list
-- @param popularTags     - top 25 popular tags
-- @param totalResults    - total amount of results
-- @param searchPanelData - data for rendering the search panel
-- @param pagerData       - data for rendering the pager
renderProgramListView
  :: [HtmlExp] -> HtmlExp -> [Program] -> [Tag] -> Int -> SearchPanelData
  -> PagerData -> View
renderProgramListView header 
                      breadcrumb 
                      results
                      popularTags
                      totalResults
                      searchPanelData@(_,_,searchSettings)
                      (currPage,totalPages,url@(path,qStr)) =
  renderBrowser
    ([ol [class "breadcrumb"]
        [li [] [a [href "#"] [text "Browser"]]
        ,breadcrumb]]
  ++(renderStdSidebar searchPanelData popularTags (showPath url)))
    [h3 [] $
      header
    ++[small [] [text $ "("++(show totalResults)++" result"++mS++")"]]]
    [text $ showSearchSettings searchSettings]
    [renderProgramList noResultsMsg results True]
    [if null results then empty else renderPager]
 where 
    mS = if totalResults == 1 then "" else "s"
    renderPager =
      (ul [class "pager"] `withId` "pager")
        [li [class $ "previous"++(mDisabled $ currPage<=1||currPage>totalPages)] 
          [prevLink]
        ,li []
          [p [class "text-muted"]
            [text "("
            ,glyphicon "file"
            ,text $ " Page "++cpStr++" of "++tpStr++")"]]
        ,li [class $ "next"    ++(mDisabled $ currPage<1||currPage>=totalPages)]
          [nextLink]]
    mDisabled b = if b then " disabled" else ""
    prevLink    = a [href prevUrl] [previousIcon,text " Prev"]
    nextLink    = a [href nextUrl] [text "Next ",nextIcon]
    prevUrl     = showUrl (path,prevQStr)
    nextUrl     = showUrl (path,nextQStr) 
    prevQStr    = delete ("page",cpStr) qStr++[("page",show $ currPage-1)]
    nextQStr    = delete ("page",cpStr) qStr++[("page",show $ currPage+1)]
    cpStr       = show currPage
    tpStr       = show totalPages
    noResultsMsg =
      if currPage>totalPages && totalPages/=0
      then "Oops, this page does not exist."
      else "Sorry, no results were found that match your search query."

-- The list view for all tags in the database.
-- @param allTags     - the list of all tags
-- @param popularTags - top 25 popular tags
tagList :: [Tag] -> [Tag] -> View
tagList allTags popularTags =
  renderBrowser
    [ol [class "breadcrumb"]
      [li [] [a [href dashboardUrl] [text "Browser"]]
      ,li [class "active"] [text "Tags"]]
    ,renderSidebarPanel
      [h4 [] [tagsIcon,text " Top 25 tags"]]
      [text "Click on a tag to search all related programs."]
      (renderTagList $ take 25 popularTags)]
    [h3 [] 
      [tagsIcon,text " All tags "
      ,small [] [text $ "("++show totalTags++" in total)"]]]
    [text "Click on a tag to search all related programs."]
    (renderTagListPartitions tagListPartitions)
    []
  where
    totalTags = length allTags
    tagListPartitions = partitionTagListByInitial allTags
    partitionTagListByInitial [] = []
    partitionTagListByInitial tags@(t:_) =
      let init     = head . tagName
          currInit = init t
          (p1,p2)  = partition ((currInit==) . init) tags
       in (toUpper currInit,p1):partitionTagListByInitial p2
    renderTagListPartitions =
      concatMap (\(i,ts) -> 
        [b [] [text $ " "++[i]++" "],br []]++(renderTagList ts)++[br []])

--- The program page which shows all information to a given program.
--- @param progVers    - program and displayed version
--- @param makeVisible - controller that makes the program visible
--- @param addToFavs   - controller that adds the program to the favorites of
---   the currently authenticated user
--- @param remFromFavs - controller that removes the program from the favorites
---   of the currently authenticated user
--- @param modifyProg  - controller that modifies title of the program
--- @param deleteProg  - controller that deletes the program
--- @param createCom   - controller that creates a comment
--- @param authzData   - the current authorization data
programPage
  :: (Program,Int)
  -> (Program -> Controller)
  -> (Program -> Controller)
  -> (Program -> Controller)
  -> (Program -> Controller)
  -> (Program -> Controller)
  -> ((String,Program) -> Controller)
  -> AuthZData
  -> View
programPage (prog,versNum)
            makeVisible
            addToFavs
            remFromFavs
            modifyProg
            deleteProg
            createCom
            authzData =
  renderBrowser
    [renderOptionsPanel
      [orangeLinkBtn (openProgramBaseUrl++key)
        [openIcon,text " Open with SmapIE",br []
        ,span [class "small"] [text "(latest version)"]]
      `addId` "open-button"
      ,mMakeVisibleOption
      ,mAddFavOption
      ,mRemFavOption
      ,mModifyOption
      ,mDeleteOption
      ,linkLinkBtn ("?download/"++key ++ (if versNum==0 then "" else '/':show versNum))
                   [downloadIcon,text " Download source code"]
         `addAttr` ("target","_blank")
      ]
    ,renderSidebarPanel
      [h4 [class "left"]  [versionIcon,text " Versions"]
      ,h4 [class "right"] [versCountHExp]]
      [text "Choose the version you want to view."]
      [label [] [text "Current version:"]
      ,select [class "form-control input-sm"] _ versMenu initVersSel
      `addId` "version-select"
      ,label [] [text "Version message:"]
      ,div [class "well well-sm"] [code [] [text versMsg]]]
    ,renderSidebarPanel
      [h4 [class "left"]  [tagsIcon,text " Tags"]
      ,h4 [class "right"] [tagCountHExp]]
      [text "Click on a tag to show all related programs."]
      (renderTagList tags)
    ,renderSidebarPanel
      [h4 [class "left"]  [commentIcon,text " Comments"]
      ,h4 [class "right"] [comCountHExp]]
      [text commentInfo]
      ltstComHExps]
    [h3 [class "left"] 
      [titleIcon,text $ " "++title,notVisibleHExp]
    ,h3 [class "right"] 
      [small [] [text "implemented in "]
      ,labelDefault [text implLangName]]]
    [div [class "left"]
      authorDatesHExps
    ,div [class "right"]
      faverCountH]
    [p [] [descriptionIcon,bold [text " Description: "],descrHExp]
    ,label [] 
      [codeIcon,text " Source code ",codeLabelHExp,text " "]
      ,codeHExp
      ,script [] [text codeMirrorInstance]
    ,modifyModal
    ,commentModal
    ,confirmMakeVisibleDialog
    ,confirmDeleteDialog]
    []    
  where
    titleRef,descrRef,comRef free
    -- getting program attributes
    key          = showProgramKey prog
    title        = programTitle prog
    descr        = programDescription prog
    isVisible    = programIsVisible prog
    implLangName = languageName $ programImplLang prog
    authorName   = userName $ programAuthor prog
    vers         = programVersions prog
    currVers     = vers !! (versNum-1)
    versCode     = versionSourceCode currVers
    versMsg      = versionMessage currVers
    versCount    = length vers
    fstVersDate  = toDayString $ versionDate $ programFirstVersion prog
    ltstVersDate = toDayString $ versionDate $ programLatestVersion prog
    tags         = programTags prog
    tagCount     = length tags
    coms         = programComments prog
    comCount     = length coms
    mltstCom     = programLatestComment prog
    favers       = programFavoriters prog
    faverCount   = length $ favers
    currUserName = maybe "" id $ getUsernameFromAuthZData authzData
    makeVisibleHdlr _ = next $ makeVisible $ setProgramIsVisible True prog
    addFavHdlr      _ = next $ addToFavs   prog
    remFavHdlr      _ = next $ remFromFavs prog
    modifyHdlr    env = next $ modifyProg $ setProgramTitle (env titleRef)
                             $ setProgramDescription (env descrRef) prog
    deleteHdlr      _ = next $ deleteProg  prog
    creatComHdlr  env = next $ createCom   (env comRef,prog)
    -- HTML expressions for program attributes
    descrHExp
      = span [class "text-muted"] [text $ if null descr then "-" else descr]
    notVisibleHExp
      = if isVisible then empty else notVisibleIcon
    authorDatesHExps 
      = let mMod = if versCount==1 then empty else ltstVersDateHExp
         in [userIcon
            ,text $ " Created by "++authorName++" on "++fstVersDate
            ,mMod]
    ltstVersDateHExp
      = text $ " (last modified on "++ltstVersDate++")"
    codeLabelHExp 
      = if versNum==versCount
        then text "(Latest version):"
        else text $ "(Version "++show versNum++" of "++show versCount++"):"
    codeHExp
      = textarea [class "form-control"] _ versCode `addId` "editor"
    versCountHExp
      = let mS = if versCount>1 then "s" else ""
         in badge [text $ show versCount++" version"++mS]
    versMenu 
      = map (\v -> 
        let num  = show $ versionNumber v
            date = toDayString $ versionDate v
         in ("Version "++num++" from "++date,"?browser/"++key++"/"++num))
        vers
    initVersSel
      = "?browser/"++key++"/"++(show versNum)
    tagCountHExp
      = badge [text $ show tagCount++" tags"]
    comCountHExp
      = let mS = if comCount==1 then "" else "s"
         in a [href "#",modalToggle,targetId "comment-modal"]
              [badge [text $ show comCount++" comment"++mS++" &raquo;"]]
    ltstComHExps
      = maybe [p [class "text-muted small"] [text "No comments yet."]]
              (\ltstCom ->
              [label [] [text "Latest comment"]
              ,div [class "well well-sm"]
                [text $ "\""++commentText ltstCom++"\""]
              ,span [class "text-muted small right"]
                [text $ "on "++(toDayString $ commentDate ltstCom)]])
              mltstCom
    faverCountH = 
      let (mYou,cnt) = if currUserName `favorites` prog
                       then (" You and ",faverCount-1)
                       else (" ",faverCount)
          msgPrefix  = mYou++show cnt++(if cnt==1 then " user " else " users ")
       in [favoriteIcon,text $ msgPrefix++"favorited this"]
    -- modify modal
    modifyModal =
      stdModal "modify-modal" "comment-modal-title" 
        [text "Modify metadata: "]
        mModifyForm
        []
    -- comment modal
    commentModal =
      stdModal "comment-modal" "comment-modal-title" 
        [commentIcon,text " Comments"]
        (mCommentForm
      ++[hr []
        ,if comCount==0
         then p [class "text-muted small"] [text "No comments yet."]
         else renderCommentList coms])
        []
    -- authorized options
    mMakeVisibleOption =
      byAuthorization (browserOperation (MakeVisible prog) authzData)
        makeVisibleOption (\_ -> empty)
    (makeVisibleOption,confirmMakeVisibleDialog) = withConfirmation 1
      (linkLinkBtn "#" [visibleIcon,text " Make program visible"])
      confirmMakeVisibleMsg makeVisibleHdlr
    mAddFavOption = 
      byAuthorization (browserOperation (AddToFavorites prog) authzData)
        addFavOption (\_ -> empty)
    addFavOption = 
      linkSubmitBtn addFavHdlr 
        [favoriteIcon,text " Add to favorites"]
    mRemFavOption = 
      byAuthorization (browserOperation (RemoveFromFavorites prog) authzData)
        remFavOption (\_ -> empty)
    remFavOption = 
      linkSubmitBtn remFavHdlr 
        [favoriteIcon,text " Remove from favorites"]

    mModifyOption = 
      byAuthorization (browserOperation (ModifyProgram prog) authzData)
        (linkLinkBtn "#" [modifiedIcon, text $ " Change title/description"]
          `addAttrs` [modalToggle,targetId "modify-modal"])
        (\_ -> empty)
    mModifyForm = 
      byAuthorization (browserOperation (ModifyProgram prog) authzData)
        [label [] [text "Title"]
        ,textarea [class "form-control",rows 1] titleRef title
        ,label [] [text "Description"]
        ,textarea [class "form-control",rows 5] descrRef descr
        ,blueSubmitBtn modifyHdlr [text "Change!"]
        ,buttonButton [class "btn btn-default",modalDismiss]
          [text "Cancel"]]
        (\err -> [p [class "text-muted"] [text err]])

    mDeleteOption = 
      byAuthorization (browserOperation (DeleteProgram prog) authzData)
        deleteOption
        (\_ -> empty)
    (deleteOption,confirmDeleteDialog) = withConfirmation 2
      (linkLinkBtn "#" [deleteIcon,text " Delete this program"])
      confirmDeleteMsg deleteHdlr
    mCommentForm =
      byAuthorization (browserOperation CreateComment authzData)
        [label [] [text "Write a new comment"]
        ,textarea [class "form-control",rows 5] comRef ""
        `addId` "comment-text-input"
        ,blueSubmitBtn creatComHdlr [text "Submit!"] `addAttr` disabled
        `addId` "comment-submit-button"
        ,buttonButton [class "btn btn-default",modalDismiss]
          [text "Cancel"]]
        (\err -> [p [class "text-muted"] [text err]])
    codeMirrorInstance =
      "var code = CodeMirror.fromTextArea(document.getElementById('editor'), "++
      "{\n"++
      "  lineNumbers: true,\n"++
      "  lineWrapping: true,\n"++
      "  mode: '"++map toLower implLangName++"',\n"++
      "  readOnly: 'nocursor',\n"++
      "  theme: 'smap'\n"++
      "});\n"
    commentInfo =
      "Click on the badge to open the comment window for viewing and writing "++
      "comments."
    confirmDeleteMsg = 
      "Are you sure you want to delete this program? This operation cannot be"++
      " undone."
    confirmMakeVisibleMsg =
      "Are you sure you want make this program available for other users? Thi"++
      "s operation currently cannot be undone."

--------------------------------------------------------------------------------
-- Standard Browser HTML components                                           --
--------------------------------------------------------------------------------

-- The standard frame rendering for Browser views.
-- @param sidebar - content of the sidebar
-- @param header  - content of the main panel header
-- @param info    - content of the info block beneath the header
-- @param body    - content of the main panel body
-- @param footer  - content of the main panel footer
renderBrowser 
  :: [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> [HtmlExp]
renderBrowser sidebar header info body footer =
  [(container `withId` "browser")
    [row
      [col [Xs 3]
        sidebar
      ,col [Xs 9]
        [(panelDefault `withId` "main-panel")
          [panelBody
            [pageHeader
              header
            ,mInfo
            ,div [class "body"] body]
          ,panelFooter
            footer]]]]]
  where
    mInfo = if null info then empty else div [class "info"] info
 
-- Standard rendering for sidebar panels with a header, an info line beneath the
-- header and a body.
-- @param header - content of the sidebar panel header
-- @param info   - content of the info block beneath the header
-- @param body   - content of the sidebar panel body
renderSidebarPanel :: [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> HtmlExp
renderSidebarPanel header info body =
  (panelDefault `withAddClass` "sidebar-panel")
    [panelBody $
      [(pageHeader `withAddClass` "header")
        header
      ,mInfo
      ,div [class "body"] body]]
  where
    mInfo = if null info then empty else div [class "info"] info

-- Standard rendering for the whole sidebar.
-- @param searchPaneldata - data for rendering the search panel
-- @param popular Tags    - top 25 popular tags
-- @param baseUrl         - the base URL for program searches (depends on the
--   type of program list, e.g. user programs)
renderStdSidebar :: SearchPanelData -> [Tag] -> String -> [HtmlExp]
renderStdSidebar searchPanelData popularTags baseUrl =
  [renderSearchPanel searchPanelData baseUrl
  ,renderSidebarPanel
    [h4 [class "left"] [tagsIcon,text " Top 25 tags"]
    ,a [href allTagsBaseUrl,class "right"] [text "all tags &raquo;"]]
    [text "Click on a tag to search all related programs."]
    (renderTagList $ take 25 popularTags)]

-- Standard rendering for the options panel.
-- @param options - contained options
renderOptionsPanel :: [HtmlExp] -> HtmlExp
renderOptionsPanel options =
  renderSidebarPanel
    [h4 [] [optionsIcon,text " Options"]]
    []
    [renderOptions]
  `addId` "options-panel"
  where
    renderOptions = ul [class "list-unstyled"] $ 
      filter (not . (HtmlStruct "li" [] [HtmlText ""]==)) $ 
      map (\o -> li [] [o]) options

-- The standard renderung for the search panel.
-- @param searchPanelData - initial data for the input elements
-- @param baseUrl         - the base url for building the query string
renderSearchPanel :: SearchPanelData -> String -> HtmlExp
renderSearchPanel (langs,sortMenu,(mQ,(t,d,ts),mLang,mSort,mOrder)) baseUrl =
  renderSidebarPanel
    [h4 [] [searchIcon,text " Advanced search"]]
    []
    [span [class "text-muted"] [filterIcon,text " Filtering options"]
    ,div [class "form-group"]
      [label [] [text "Filter by keyword"]
      ,input [("type","text")
             ,class "form-control input-sm"
             ,value keyword
             ,placeholder "Keyword"]
      `addId` "search-panel-keyword-input"]
    ,label [] [text "Search keyword in"]
    ,div [class "checkbox"]
      [label []
        [input (("type","checkbox"):value "title":mChecked t)
        `addId` "search-panel-title-checkbox"]
        ,titleIcon,text " Title"]
    ,div [class "checkbox"]
      [label []
        [input (("type","checkbox"):value "descr":mChecked d)
        `addId` "search-panel-descr-checkbox"]
        ,descriptionIcon,text " Description"]
    ,div [class "checkbox"]
      [label []
        [input (("type","checkbox"):value "tags":mChecked ts)
        `addId` "search-panel-tags-checkbox"]
        ,tagsIcon,text " Tags"]
    ,label [] [text " Filter by language"]
    ,select [class "form-control input-sm"] _ langMenu lang
    `addId` "search-panel-lang-select"
    ,hr []
    ,span [class "text-muted"] [sortIcon,text " Sorting options"]
    ,label [] [text "Sort by"]
    ,select [class "form-control input-sm"] _ sortMenu sort
    `addId` "search-panel-sort-select"
    ,label [] [text "Order results"]
    ,select [class "form-control input-sm"] _ orderMenu order
    `addId` "search-panel-order-select"
    ,div [class "btn-group"]
      [greyLinkBtn baseUrl [searchIcon,text " Search"]
      `addId` "search-panel-apply-button"
      ,greyLinkBtn "#" [resetIcon,text " Reset"]
      `addId` "search-panel-reset-button"]]
  `addId` "search-panel"
  where
    langMenu   = ("All languages (default)","")
                 :map (\langName -> (langName,map toLower langName)) 
                      (map languageName langs)
    orderMenu  = [("descending (default)","desc"),("ascending","asc")]
    keyword = maybe ""        id mQ
    lang    = maybe ""        id mLang
    sort    = maybe "created" id mSort
    order   = maybe "desc"    id mOrder    
    mChecked b = if b then [checked] else []

--------------------------------------------------------------------------------
-- Rendering application entities for browser views                           --
--------------------------------------------------------------------------------

-- Renders the program list.
-- @param noProgsMsg - message for the case of an empty program list
-- @param progs      - the program list
-- @param detailed   - flag to display more details in the program list
renderProgramList :: String -> [Program] -> Bool -> HtmlExp
renderProgramList noProgsMsg progs detailed =
  if null progs
  then span [class "text-muted small"] 
         [warningMessageIcon,text $ ' ':noProgsMsg]
  else (ul [class "list-unstyled"] `withId` "program-list") $
         map (li [] . renderProgramInList detailed) progs

-- Renders a single program in the program list.
-- @param detailed - flag to display more details in the program list
-- @param prog     - the program
renderProgramInList :: Bool -> Program -> [HtmlExp]
renderProgramInList detailed prog =
  [div [class "left"] $
    [h5 [] [titleHExp,visibilityHExp,text " ",openLink]
    ,langNameHExp]
    ++(text " ":tagNameLinks)
  ,div [class "right"] $
    ltstVersDateHexps
  ++fstVersDateHExps
  ++faverCountHExps
  ++[br []]
  ++authorHExps]
  where
    title        = programTitle                       prog
    isVisible    = programIsVisible                   prog
    langName     = languageName $ programImplLang     prog
    author       = userName $ programAuthor           prog
    fstVersDate  = versionDate $ programFirstVersion  prog
    ltstVersDate = versionDate $ programLatestVersion prog
    tags         = programTags                        prog
    faverCount   = length $ programFavoriters         prog
    key          = showProgramKey                     prog
    titleHExp    = a [href $ progInBrowserBaseUrl++"/"++key]
                     [text title]
    openLink     = a [href $ openProgramBaseUrl++key,class "open-link"]
                     [openIcon,text " open"]
    visibilityHExp = if isVisible then empty else notVisibleIcon
    langNameHExp = labelWarning [text langName]
    tagNameLinks = 
      let count = if detailed then 5 else 3
          mDots = if (length tags) > count 
                  then [span [class "small"] [text "..."]]
                  else [empty]
       in (renderTagList $ take count tags)++mDots
    fstVersDateHExps = 
      if detailed
      then [createdIcon,text $ " "++showCalendarTime fstVersDate++" "]
      else []
    ltstVersDateHexps =
      if detailed
      then if fstVersDate == ltstVersDate
           then []
           else [modifiedIcon,text $ " "++showCalendarTime ltstVersDate++" "]
      else []
    faverCountHExps = 
      [favoriteIcon,text $ " "++show faverCount++" "]
    authorHExps = 
      if detailed
      then [userIcon,text $ " "++author]
      else []
    showCalendarTime (CalendarTime y m d _ _ _ _) =
      show y++"/"++show m++"/"++show d

-- Renders a tag list as a list of hyperlinks.
renderTagList :: [Tag] -> [HtmlExp]
renderTagList = concatMap $ \tag ->
  [a 
    [href $ allProgramsBaseUrl++"?q="++tagName tag++"&amp;targets=tags"
    ,class "small"]
    [text $ "["++tagName tag++"]"]
  ,text " "]

--  Renders the comment list for the program page view.
renderCommentList :: [Comment] -> HtmlExp
renderCommentList =
  ul [class "list-unstyled"] . reverse . (map $ \(n,c) ->
    li []
      [label [class "left"]
        [commentIcon,text $ " Comment #"++show n]
      ,span [class "text-muted small right"]
        [text $ "on "++(toDayString $ commentDate c)]
      ,div [class "comment-text"]
        [text $ "\""++commentText c++"\""]]) . zip [1..]

--------------------------------------------------------------------------------
-- Helper types and operations on search settings                             --
--------------------------------------------------------------------------------

--- Type synonym shortcut for possibly adjusted settings for a program search.
--- A search settings tuple consists of
--- - a possibly given keyword
--- - a triple specifying the search targets (title, description and tags)
--- - a possibly given language name
--- - a possibly given sorting specificatiom
--- - a possibly given order spcification.
type SearchSettings
  = (Maybe String,(Bool,Bool,Bool),Maybe String,Maybe String,Maybe String)

--- Data for rendering the search panel. A search panel data tuple consists of
--- - the list of available languages
--- - the sorting options menu
--- - the current search settings
type SearchPanelData
  = ([Language],[(String,String)],SearchSettings)

--- Data for rendering the pager. A pager data tuple consists of
--- - the current page
--- - the total number of pages
--- - the current URL
type PagerData
  = (Int,Int,Url)

--- The default search settings.
defaultSearchSettings :: SearchSettings
defaultSearchSettings = (Nothing,(True,True,True),Nothing,Nothing,Nothing)

--- Returns the string representation of the current search settings.
showSearchSettings :: SearchSettings -> String
showSearchSettings (mQ,_,mLang,mSort,mOrder) =
  (if null keyword && null lang 
  then "...with no filter applied "
  else "..."++(unwords $ keyInTargetsFilter++mAnd++langFilter)++" ")++
  "(sorted by "++sorting++", "++ordering++")."
  where
    keyword = maybe ""        id mQ
    lang    = maybe ""        id mLang
    sort    = maybe "created" id mSort
    order   = maybe "desc"    id mOrder
    mAnd = 
      if length (keyInTargetsFilter++langFilter)==2
      then ["and"]
      else []
    keyInTargetsFilter =
      if null keyword 
      then []
      else ["with keyword \""++keyword++"\""]
    langFilter =
      if null lang
      then []
      else ["implemented in "++lang]
    sorting = case sort of
      "created"    -> "creation date"
      "modified"   -> "last modified date"
      "popularity" -> "popularity"
      "title"      -> "title"
      "lang"       -> "language"
    ordering = case order of
      "asc"        -> "ascending"
      "desc"       -> "descending"

--------------------------------------------------------------------------------