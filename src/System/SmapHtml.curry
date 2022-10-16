--------------------------------------------------------------------------------
--- This module provides application-specific HTML components and elements for
--- the basic layout, WUI forms and views.
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version October 2022
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
  blueSubmitBtn,greenSubmitBtn,orangeSubmitBtn,linkSubmitBtn,greyCancelBtn,
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

import Data.Char
import Data.List
import Prelude hiding (div,span,empty)
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
viewportMetaTag :: PageParam
viewportMetaTag = PageHeadInclude $
  meta [name "viewport",content "width=device-width, initial-scale=1.0"]

--- The favicon.
favicon :: PageParam
favicon = PageHeadInclude $
  htmlStruct "link" [rel "shortcut icon",href "favicon.ico"] []

--- JavaScript files to be included in the head (especially CodeMirror modes).
--- @param langNames - the names of all languages in the database
jsHeadIncludes :: [String] -> [PageParam]
jsHeadIncludes langNames = map (\file -> PageJScript $ "js/"++file++".js") $
  ["codemirror/codemirror"
  ,"codemirror/addons/active-line"
  ,"codemirror/addons/closebrackets"
  ,"codemirror/addons/matchbrackets"]
  ++map (\langName -> "codemirror/modes/"++map toLower langName) langNames

--- CSS files to be included in the head.
cssIncludes :: [PageParam]
cssIncludes = map (\file -> PageCSS $ "css/"++file++".css")
  ["bootstrap/bootstrap.min"
  ,"bootstrap/bootstrap-theme.min"
  ,"codemirror/codemirror"
  ,"codemirror/themes/smap"
  ,"smap"]

--- JavaScript files to be included in the body (for performance reasons).
jsBodyIncludes :: HTML h => [h]
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
renderNavbar :: HTML h => Url -> [Language] -> Maybe AuthNData -> h
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
       in li [] [a [href $ newProgBaseUrl ++ "/" ++ map toLower langName]
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
          [modifiedIcon,text " Edit systems"]]
      ,li []
        [a [href "?users/list"] 
          [browserIcon,text " Show users"]]]
    mAdmin b = if b then " admin" else ""
    navbarSearchTooltip =
      "Search all programs on Smap that contain the given keyword in either t"++
      "heir title, description or tags!"

--- HTML wrapper element which wraps the navigation bar and the body content to
--- enable the sticky footer.
--- @param attrs   - attributes to modify the wrapper
--- @param content - the wrapped content above the sticky footer
wrap :: HTML h => [HtmlAttr] -> [h] -> h
wrap attrs content = 
  div attrs content 
  `addId` "wrap" 

--- The sticky footer.
stickyFooter :: HTML h => h
stickyFooter = 
  (div [] `withId` "sticky-footer")
    [container
      [p [classA "text-muted"]
        [text "&copy; 2014-2022, Lasse Kristopher Meyer, Michael Hanus &bull; "
        ,a [href "?about"] [text " About"]]]]

-- A panel with a width (in columns between 2 and 12), title, body, and footer.
panelWith :: HTML h => Int -> [h] -> [h] -> [h] -> h
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
smTextInput :: HTML h => String -> h
smTextInput pholder = 
  input [("type","text"),classA "form-control input-sm",placeholder pholder]

--------------------------------------------------------------------------------
-- Buttons                                                                    --
--------------------------------------------------------------------------------

-- Link buttons

--- A hyperlink rendered as a grey button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
greyLinkBtn :: HTML h => String -> [h] -> h
greyLinkBtn url = a [href url,classA "btn btn-default"]

--- A hyperlink rendered as a small blue button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smBlueLinkBtn :: HTML h => String -> [h] -> h
smBlueLinkBtn url = a [href url,classA "btn btn-primary btn-sm"]

--- A hyperlink rendered as a blue button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
blueLinkBtn :: HTML h => String -> [h] -> h
blueLinkBtn url = a [href url,classA "btn btn-primary"]

--- A hyperlink rendered as a small green button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smGreenLinkBtn :: HTML h => String -> [h] -> h
smGreenLinkBtn url = a [href url,classA "btn btn-success btn-sm"]

--- A hyperlink rendered as an extra small default button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
xsDefaultLinkBtn :: HTML h => String -> [h] -> h
xsDefaultLinkBtn url = a [href url,classA "btn btn-default btn-xxs"]

--- A hyperlink rendered as a green button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
greenLinkBtn :: HTML h => String -> [h] -> h
greenLinkBtn url = a [href url,classA "btn btn-success"]

--- A hyperlink rendered as a small orange button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
smOrangeLinkBtn :: HTML h => String -> [h] -> h
smOrangeLinkBtn url = a [href url,classA "btn btn-warning btn-sm"]

--- A hyperlink rendered as a orange button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
orangeLinkBtn :: HTML h => String -> [h] -> h
orangeLinkBtn url = a [href url,classA "btn btn-warning"]

--- A hyperlink rendered as a link button.
--- @param url   - the hyperlink URL 
--- @param label - label HTML expressions
linkLinkBtn :: HTML h => String -> [h] -> h
linkLinkBtn url = a [href url,classA "btn btn-link"]

-- Submit buttons

--- A submit button rendered as a blue button.
--- @param label - the button label
--- @param hdlr - the handler
blueSubmitBtn :: String -> HtmlHandler -> HtmlExp
blueSubmitBtn = formSubmitButton [classA "btn btn-primary"]

--- A submit button rendered as a blue button.
--- @param label - the button label
--- @param hdlr - the handler
greenSubmitBtn :: String -> HtmlHandler -> HtmlExp
greenSubmitBtn = formSubmitButton [classA "btn btn-success"]

--- A submit button rendered as a orange button.
--- @param hdlr  - the HTML handler
--- @param label - label HTML expressions
orangeSubmitBtn :: String -> HtmlHandler -> HtmlExp
orangeSubmitBtn = formSubmitButton [classA "btn btn-warning"]

--- A submit button rendered as a link button.
--- @param hdlr  - the HTML handler
--- @param label - label HTML expressions
linkSubmitBtn :: String -> HtmlHandler -> HtmlExp
linkSubmitBtn = formSubmitButton [classA "btn btn-link"]

-- Standard buttons

--- A grey cancel button.
greyCancelBtn :: HTML h => h
greyCancelBtn = a [href landingPageUrl,classA "btn btn-default"] [text "Cancel"]

--------------------------------------------------------------------------------
-- Icons                                                                      --
--------------------------------------------------------------------------------

aboutIcon :: HTML h => h
aboutIcon = glyphicon "info-sign"

addIcon :: HTML h => h
addIcon = glyphicon "plus-sign"

browserIcon :: HTML h => h
browserIcon = glyphicon "list"

codeIcon :: HTML h => h
codeIcon = glyphicon "file"

commentIcon :: HTML h => h
commentIcon = glyphicon "comment"

createdIcon :: HTML h => h
createdIcon = glyphicon "calendar"

dashboardIcon :: HTML h => h
dashboardIcon = glyphicon "dashboard"

deleteIcon :: HTML h => h
deleteIcon = glyphicon "trash"

descriptionIcon :: HTML h => h
descriptionIcon = glyphicon "align-left"

downloadIcon :: HTML h => h
downloadIcon = glyphicon "download"

uploadIcon :: HTML h => h
uploadIcon = glyphicon "upload"

executionIcon :: HTML h => h
executionIcon = glyphicon "cog"

execErrorIcon :: HTML h => h
execErrorIcon = glyphicon "exclamation-sign"

execSuccessIcon :: HTML h => h
execSuccessIcon = glyphicon "ok-sign"

favoriteIcon :: HTML h => h
favoriteIcon = glyphicon "heart"

filterIcon :: HTML h => h
filterIcon = glyphicon "filter"

helpIcon :: HTML h => h
helpIcon = glyphicon "question-sign"

idIcon :: HTML h => h
idIcon = glyphicon "tag"

infoIcon :: HTML h => h
infoIcon = glyphicon "info-sign"

languageIcon :: HTML h => h
languageIcon = glyphicon "pencil"

modifiedIcon :: HTML h => h
modifiedIcon = glyphicon "pencil"

nextIcon :: HTML h => h
nextIcon = glyphicon "chevron-right"

notVisibleIcon :: HTML h => h
notVisibleIcon = glyphicon "eye-close"

openIcon :: HTML h => h
openIcon = glyphicon "share"

optionsIcon :: HTML h => h
optionsIcon = glyphicon "list-alt"

passwordIcon :: HTML h => h
passwordIcon = glyphicon "lock"

previousIcon :: HTML h => h
previousIcon = glyphicon "chevron-left"

programsIcon :: HTML h => h
programsIcon = glyphicon "list"

recentIcon :: HTML h => h
recentIcon = glyphicon "time"

reloadIcon :: HTML h => h
reloadIcon = glyphicon "retweet"

resetIcon :: HTML h => h
resetIcon = glyphicon "refresh"

saveIcon :: HTML h => h
saveIcon = glyphicon "floppy-save"

searchIcon :: HTML h => h
searchIcon = glyphicon "search"

signInIcon :: HTML h => h
signInIcon = glyphicon "log-in"

signOutIcon :: HTML h => h
signOutIcon = glyphicon "log-out"

signUpIcon :: HTML h => h
signUpIcon = glyphicon "pencil"

smapIcon :: HTML h => h
smapIcon = glyphicon "edit"

smapIEIcon :: HTML h => h
smapIEIcon = glyphicon "edit"

sortIcon :: HTML h => h
sortIcon = glyphicon "sort"

tagsIcon :: HTML h => h
tagsIcon = glyphicon "tags"

titleIcon :: HTML h => h
titleIcon = glyphicon "bookmark"

userIcon :: HTML h => h
userIcon = glyphicon "user"

userFavoritesIcon :: HTML h => h
userFavoritesIcon = glyphicon "heart"

userProgramsIcon :: HTML h => h
userProgramsIcon = glyphicon "th-list"

versionIcon :: HTML h => h
versionIcon = glyphicon "random"

visibleIcon :: HTML h => h
visibleIcon = glyphicon "eye-open"

warningMessageIcon :: HTML h => h
warningMessageIcon = glyphicon "warning-sign"

--------------------------------------------------------------------------------
--- Standard table in Smap.
spTable :: HTML h => [[[h]]] -> h
spTable items = table items  `addClass` "table table-hover table-condensed"

