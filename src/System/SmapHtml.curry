--------------------------------------------------------------------------------
--- This module provides application-specific HTML components and elements for
--- the basic layout, WUI forms and views.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version November 2018
--------------------------------------------------------------------------------

module System.SmapHtml (
  module HTML.Bootstrap3, module HTML.Html5,
  viewportMetaTag,favicon,jsHeadIncludes,cssIncludes,jsBodyIncludes,
  renderNavbar,wrap,stickyFooter,panelWith,
  landingPageUrl,newProgBaseUrl,openProgramBaseUrl,dashboardUrl,
  progInBrowserBaseUrl,allProgramsBaseUrl,userProgramsBaseUrl,
  userFavoritesBaseUrl,allTagsBaseUrl,
  smTextInput,
  greyLinkBtn,blueLinkBtn,smBlueLinkBtn,smGreenLinkBtn,xsDefaultLinkBtn,
  greenLinkBtn,orangeLinkBtn,linkLinkBtn,
  blueSubmitBtn,orangeSubmitBtn,linkSubmitBtn,greyCancelBtn,
  aboutIcon,addIcon,browserIcon,codeIcon,commentIcon,createdIcon,dashboardIcon,
  deleteIcon,descriptionIcon,downloadIcon,uploadIcon,
  executionIcon,execErrorIcon,
  execSuccessIcon,favoriteIcon,filterIcon,helpIcon,idIcon,infoIcon,languageIcon,
  modifiedIcon,nextIcon,notVisibleIcon,openIcon,optionsIcon,passwordIcon,
  previousIcon,recentIcon,reloadIcon,resetIcon,saveIcon,searchIcon,signInIcon,
  signUpIcon,smapIEIcon,sortIcon,tagsIcon,titleIcon,userIcon,userFavoritesIcon,
  userProgramsIcon,versionIcon,visibleIcon,warningMessageIcon,
  spTable
) where

import Char
import List
import Prelude hiding (div,span)
import qualified Prelude(div)

import HTML.Html5 hiding (id)
import HTML.Bootstrap3
import Model.ExecEnv
import System.Authentication
import System.Url

--------------------------------------------------------------------------------
-- Form parameters and includes                                               --
--------------------------------------------------------------------------------

--- The viewport meta tag (http://getbootstrap.com/css/#overview-mobile).
viewportMetaTag :: FormParam
viewportMetaTag = HeadInclude $
  meta [name "viewport",content "width=device-width, initial-scale=1.0"]

--- The favicon.
favicon :: FormParam
favicon = HeadInclude $
  HtmlStruct "link" [rel "shortcut icon",href "favicon.ico"] []

--- JavaScript files to be included in the head (especially CodeMirror modes).
--- @param langNames - the names of all languages in the database
jsHeadIncludes :: [String] -> [FormParam]
jsHeadIncludes langNames = map (\file -> FormJScript $ "js/"++file++".js") $
  ["codemirror/codemirror"
  ,"codemirror/addons/active-line"
  ,"codemirror/addons/closebrackets"
  ,"codemirror/addons/matchbrackets"]
  ++map (\langName -> "codemirror/modes/"++map toLower langName) langNames

--- CSS files to be included in the head.
cssIncludes :: [FormParam]
cssIncludes = map (\file -> FormCSS $ "css/"++file++".css")
  ["bootstrap/bootstrap.min"
  ,"bootstrap/bootstrap-theme.min"
  ,"codemirror/codemirror"
  ,"codemirror/themes/smap"
  ,"smap"]

--- JavaScript files to be included in the body (for performance reasons).
jsBodyIncludes :: [HtmlExp]
jsBodyIncludes = map (\file -> script [src $ "js/"++file++".js"] [])
  ["jquery-2.0.3.min"
  ,"bootstrap/bootstrap.min"
  ,"smap"]

--------------------------------------------------------------------------------
-- Components for the basic layout                                            --
--------------------------------------------------------------------------------

--- Renders the navigation bar (brand, links and dropdown menus).
--- @param url        - the current URL (to set the active state of links)
--- @param langs      - all languages in the database (for the SmapIE menu)
--- @param mAuthNData - the current session authentication data (if available)
renderNavbar :: Url -> [Language] -> Maybe AuthNData -> HtmlExp
renderNavbar url langNames mAuthNData =
  nav [classA "navbar navbar-inverse navbar-fixed-top",role "navigation"]
    [container
      [navbarHeader
        [navbarBrand landingPageUrl [smapIcon,text " Smap"]]
      ,div [classA "collapse navbar-collapse"]
        -- Quick search form
        [(div [classA "navbar-form navbar-left",title navbarSearchTooltip]
        `withId` "navbar-search-form")
          [div [classA "form-group"]
            [smTextInput "Quick search"
            `addId` "navbar-search-keyword-input"]
          ,smGreenLinkBtn allProgramsBaseUrl
            [searchIcon]
          `addId` "navbar-search-apply-button"]
        ,ul [classA "nav navbar-nav navbar-right"]
          -- SmapIE dropdown menu
          [li [classA "dropdown"]
            [a [href "#",classA "dropdown-toggle",dropdownToggle]
              [smapIEIcon,text " SmapIE ",caret]
            ,ul [classA "dropdown-menu"] $
              [li [classA "dropdown-header"]
                [text "New program"]]
            ++langMenu
            ++[li [classA "divider"] []
              ,li [classA "dropdown-header"]
                [text "Open program by ID"]
              ,(li [] `withId` "navbar-fork-form")
                [div [classA "input-group"]
                  [smTextInput "Program ID"
                  `addId` "navbar-fork-id-input"
                  ,div [classA "input-group-btn"]
                    [smOrangeLinkBtn openProgramBaseUrl [openIcon]
                    `addId` "navbar-fork-apply-button"]]]]]
          -- Browser dropdown menu
          ,li [classA "dropdown"]
            [a [href "#",classA "dropdown-toggle",dropdownToggle]
              [browserIcon,text " Browser ",caret]
            ,ul [classA "dropdown-menu"]
              [li [] 
                [a [href dashboardUrl] 
                  [dashboardIcon,text " Dashboard"]]
              ,li [classA "divider"] []
              ,li [classA "dropdown-header"]
                [text "Programs on Smap"]
              ,li []
                [a [href allProgramsBaseUrl]
                  [recentIcon,text " Most recent"]]
              ,li []
                [a [href $ allProgramsBaseUrl++"?sort=popularity"] 
                  [favoriteIcon,text " Most favorited"]]
              ,li [classA "divider"] []
              ,li [classA "dropdown-header"]
                [text "Tags on Smap"]
              ,li []
                [a [href allTagsBaseUrl] [tagsIcon,text " All tags"]]]]
          -- authentication/user menu
          ,maybe 
          (li (mActive "?signin") 
            [a [href "?signin"] [signInIcon,text " Sign in"]])
          (\(userName,userIsAdmin) ->
           li [classA "dropdown"]
            [a 
              [href "#"
              ,classA $ "dropdown-toggle"++(mAdmin userIsAdmin)
              ,dropdownToggle]
              [glyphicon "user",text $ " "++userName++" ",caret]
            ,ul [classA "dropdown-menu"] $
              [li [classA "dropdown-header"]
                [text "User options"]
              ,li []
                [a [href userProgramsBaseUrl] 
                  [userProgramsIcon,text " My programs"]]
              ,li []
                [a [href userFavoritesBaseUrl] 
                  [userFavoritesIcon,text " My favorites"]]
              ,li []
                [a [href changePasswordBaseUrl] 
                  [passwordIcon,text " Change my password"]]]
            ++(if userIsAdmin then adminMenu else [empty])
            ++[li [classA "divider"] 
                []
              ,li [] 
                [a [href "?signout"] [signOutIcon,text " Sign out"]]]])
          mAuthNData
          -- link to help page
          ,li (mActive "?help")
            [a [href "?help"]
              [helpIcon,text " Help"]]]]]]
  `addId` "navigation-bar"
  where
    mActive urlPrefix = 
      if urlPrefix `isPrefixOf` showUrl url then [("class","active")] else []
    langMenu = 
      map (\lang ->
      let langName = languageName lang
       in li [] [a [href $ newProgBaseUrl++"/"++map toLower langName]
            [text langName]])
      langNames
    adminMenu =
      [li [classA "divider"] []
      ,li [classA "dropdown-header"] [text "Admin options"]
      ,li []
        [a [href "?languages/new"] 
          [addIcon,text " Add language"]]
      ,li []
        [a [href "?systems/new"] 
          [addIcon,text " Add system"]]
      ,li []
        [a [href "?systems/list"] 
          [modifiedIcon,text " Edit systems"]]]
    mAdmin b = if b then " admin" else ""
    navbarSearchTooltip =
      "Search all programs on Smap that contain the given keyword in either t"++
      "heir title, description or tags!"

--- HTML wrapper element which wraps the navigation bar and the body content to
--- enable the sticky footer.
--- @param attrs   - attributes to modify the wrapper
--- @param content - the wrapped content above the sticky footer
wrap :: [HtmlAttr] -> [HtmlExp] -> HtmlExp
wrap attrs content = 
  div attrs content 
  `addId` "wrap" 

--- The sticky footer.
stickyFooter :: HtmlExp
stickyFooter = 
  (div [] `withId` "sticky-footer")
    [container
      [p [classA "text-muted"]
        [text "&copy; 2014, Lasse Kristopher Meyer &bull; "
        ,a [href "?about"] [text " About"]]]]

-- A panel with a width (in columns between 2 and 12), title, body, and footer.
panelWith :: Int -> [HtmlExp] -> [HtmlExp] -> [HtmlExp] -> HtmlExp
panelWith colwidth title body footer =
   container
    [row
      [div [classA ("col-xs-"++show selectedwidth++" col-xs-offset-"++show offset)]
        [div [classA "panel panel-info"]
          [panelHeading [h3 [classA "panel-title"] title]
          ,panelBody body
          ,if null footer then empty else panelFooter footer]]]]
 where
   selectedwidth = if colwidth > 12 then 12 else colwidth
   offset = (12 - selectedwidth) `Prelude.div` 2

--------------------------------------------------------------------------------
-- URLs                                                                       --
--------------------------------------------------------------------------------

--- The URL that leads to the landing page.
landingPageUrl :: String
landingPageUrl = "smap.cgi"

--- The base Url for starting a new SmapIE session.
newProgBaseUrl :: String
newProgBaseUrl = "?new"

--- The base URL for opening programs with SmapIE.
openProgramBaseUrl :: String
openProgramBaseUrl = "?"

--- The URL that leads to the browser dashboard.
dashboardUrl :: String
dashboardUrl = "?browser"

--- The URL for opening programs with the browser.
progInBrowserBaseUrl :: String
progInBrowserBaseUrl = "?browser"

--- Base URL for the list of all programs.
allProgramsBaseUrl :: String
allProgramsBaseUrl = "?browser/programs"

--- Base URL for a list of all user programs.
userProgramsBaseUrl :: String
userProgramsBaseUrl = "?browser/myprograms"

--- Base URL for a list of all user favorites.
userFavoritesBaseUrl :: String
userFavoritesBaseUrl = "?browser/myfavorites"

--- Base URL for a list of all user favorites.
changePasswordBaseUrl :: String
changePasswordBaseUrl = "?passwd"

--- Base URL for a list of all tags on Smap.
allTagsBaseUrl :: String
allTagsBaseUrl = "?browser/tags"

--------------------------------------------------------------------------------
-- Input fields                                                               --
--------------------------------------------------------------------------------

--- A small text input field (without CGI reference).
--- @param pholder - value of the placeholder attribute
smTextInput :: String -> HtmlExp
smTextInput pholder = 
  input [("type","text"),classA "form-control input-sm",placeholder pholder]

--------------------------------------------------------------------------------
-- Buttons                                                                    --
--------------------------------------------------------------------------------

-- Link buttons

--- A hyperlink rendered as a grey button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
greyLinkBtn :: String -> [HtmlExp] -> HtmlExp
greyLinkBtn url = a [href url,classA "btn btn-default"]

--- A hyperlink rendered as a small blue button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smBlueLinkBtn :: String -> [HtmlExp] -> HtmlExp
smBlueLinkBtn url = a [href url,classA "btn btn-primary btn-sm"]

--- A hyperlink rendered as a blue button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
blueLinkBtn :: String -> [HtmlExp] -> HtmlExp
blueLinkBtn url = a [href url,classA "btn btn-primary"]

--- A hyperlink rendered as a small green button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smGreenLinkBtn :: String -> [HtmlExp] -> HtmlExp
smGreenLinkBtn url = a [href url,classA "btn btn-success btn-sm"]

--- A hyperlink rendered as an extra small default button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
xsDefaultLinkBtn :: String -> [HtmlExp] -> HtmlExp
xsDefaultLinkBtn url = a [href url,classA "btn btn-default btn-xxs"]

--- A hyperlink rendered as a green button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
greenLinkBtn :: String -> [HtmlExp] -> HtmlExp
greenLinkBtn url = a [href url,classA "btn btn-success"]

--- A hyperlink rendered as a small orange button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smOrangeLinkBtn :: String -> [HtmlExp] -> HtmlExp
smOrangeLinkBtn url = a [href url,classA "btn btn-warning btn-sm"]

--- A hyperlink rendered as a orange button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
orangeLinkBtn :: String -> [HtmlExp] -> HtmlExp
orangeLinkBtn url = a [href url,classA "btn btn-warning"]

--- A hyperlink rendered as a link button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
linkLinkBtn :: String -> [HtmlExp] -> HtmlExp
linkLinkBtn url = a [href url,classA "btn btn-link"]

-- Submit buttons

--- A submit button rendered as a blue button.
--- @param hdlr  - the HTML handler
--- @param label - label HTML expressions
blueSubmitBtn :: HtmlHandler -> [HtmlExp] -> HtmlExp
blueSubmitBtn = submitButton [classA "btn btn-primary"]

--- A submit button rendered as a orange button.
--- @param hdlr  - the HTML handler
--- @param label - label HTML expressions
orangeSubmitBtn :: HtmlHandler -> [HtmlExp] -> HtmlExp
orangeSubmitBtn = submitButton [classA "btn btn-warning"]

--- A submit button rendered as a link button.
--- @param hdlr  - the HTML handler
--- @param label - label HTML expressions
linkSubmitBtn :: HtmlHandler -> [HtmlExp] -> HtmlExp
linkSubmitBtn = submitButton [classA "btn btn-link"]

-- Standard buttons

--- A grey cancel button.
greyCancelBtn :: HtmlExp
greyCancelBtn = a [href landingPageUrl,classA "btn btn-default"] [text "Cancel"]

--------------------------------------------------------------------------------
-- Icons                                                                      --
--------------------------------------------------------------------------------

aboutIcon :: HtmlExp
aboutIcon = glyphicon "info-sign"

addIcon :: HtmlExp
addIcon = glyphicon "plus-sign"

browserIcon :: HtmlExp
browserIcon = glyphicon "list"

codeIcon :: HtmlExp
codeIcon = glyphicon "file"

commentIcon :: HtmlExp
commentIcon = glyphicon "comment"

createdIcon :: HtmlExp
createdIcon = glyphicon "calendar"

dashboardIcon :: HtmlExp
dashboardIcon = glyphicon "dashboard"

deleteIcon :: HtmlExp
deleteIcon = glyphicon "trash"

descriptionIcon :: HtmlExp
descriptionIcon = glyphicon "align-left"

downloadIcon :: HtmlExp
downloadIcon = glyphicon "download"

uploadIcon :: HtmlExp
uploadIcon = glyphicon "upload"

executionIcon :: HtmlExp
executionIcon = glyphicon "cog"

execErrorIcon :: HtmlExp
execErrorIcon = glyphicon "exclamation-sign"

execSuccessIcon :: HtmlExp
execSuccessIcon = glyphicon "ok-sign"

favoriteIcon :: HtmlExp
favoriteIcon = glyphicon "heart"

filterIcon :: HtmlExp
filterIcon = glyphicon "filter"

helpIcon :: HtmlExp
helpIcon = glyphicon "question-sign"

idIcon :: HtmlExp
idIcon = glyphicon "tag"

infoIcon :: HtmlExp
infoIcon = glyphicon "info-sign"

languageIcon :: HtmlExp
languageIcon = glyphicon "pencil"

modifiedIcon :: HtmlExp
modifiedIcon = glyphicon "pencil"

nextIcon :: HtmlExp
nextIcon = glyphicon "chevron-right"

notVisibleIcon :: HtmlExp
notVisibleIcon = glyphicon "eye-close"

openIcon :: HtmlExp
openIcon = glyphicon "share"

optionsIcon :: HtmlExp
optionsIcon = glyphicon "list-alt"

passwordIcon :: HtmlExp
passwordIcon = glyphicon "lock"

previousIcon :: HtmlExp
previousIcon = glyphicon "chevron-left"

programsIcon :: HtmlExp
programsIcon = glyphicon "list"

recentIcon :: HtmlExp
recentIcon = glyphicon "time"

reloadIcon :: HtmlExp
reloadIcon = glyphicon "retweet"

resetIcon :: HtmlExp
resetIcon = glyphicon "refresh"

saveIcon :: HtmlExp
saveIcon = glyphicon "floppy-save"

searchIcon :: HtmlExp
searchIcon = glyphicon "search"

signInIcon :: HtmlExp
signInIcon = glyphicon "log-in"

signOutIcon :: HtmlExp
signOutIcon = glyphicon "log-out"

signUpIcon :: HtmlExp
signUpIcon = glyphicon "pencil"

smapIcon :: HtmlExp
smapIcon = glyphicon "edit"

smapIEIcon :: HtmlExp
smapIEIcon = glyphicon "edit"

sortIcon :: HtmlExp
sortIcon = glyphicon "sort"

tagsIcon :: HtmlExp
tagsIcon = glyphicon "tags"

titleIcon :: HtmlExp
titleIcon = glyphicon "bookmark"

userIcon :: HtmlExp
userIcon = glyphicon "user"

userFavoritesIcon :: HtmlExp
userFavoritesIcon = glyphicon "heart"

userProgramsIcon :: HtmlExp
userProgramsIcon = glyphicon "th-list"

versionIcon :: HtmlExp
versionIcon = glyphicon "random"

visibleIcon :: HtmlExp
visibleIcon = glyphicon "eye-open"

warningMessageIcon :: HtmlExp
warningMessageIcon = glyphicon "warning-sign"

--------------------------------------------------------------------------------
--- Standard table in Smap.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

