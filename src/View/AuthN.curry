--------------------------------------------------------------------------------
--- This module provides all views for authentication related pages (WUI forms
--- for signing in, signing up and password requests).
---
--- @author Lasse Kristopher Meyer
--- @version January 2014
--------------------------------------------------------------------------------

module View.AuthN (
  signUpPage,signInPage,forgotPasswordPage,changePasswordForm
) where

import Prelude hiding (div)

import System.Controllers
import System.SmapHtml
import System.SmapWui
import System.Views

--------------------------------------------------------------------------------
-- Exported views                                                             --
--------------------------------------------------------------------------------

--- Supplies a WUI form to sign up for Smap by creating a new user with an
--- username, an email address and a password.
--- @param signUp - the controller which handles the user creation
signUpPage
  :: (String,String,String,String)
  -> ((String,String,String,String) -> Controller)
  -> View
signUpPage initCreationData signUp = 
  renderWuiForm wSignUpData initCreationData signUp
    [h3 [] [signUpIcon,text " Sign up for Smap"]]
    []
    [text "Sign up"]
    []
    [text "Already have an account? "
    ,a [href "?signin"] [text "Sign in &raquo;"]]

--- Supplies a WUI form to sign in to Smap with a user name and a password. If
--- given, an initial user name is set.
--- @param mUsername - an possibly given initial user name 
--- @param signIn    - the controller which handles the sign in process
signInPage :: Maybe String -> ((String,String) -> Controller) -> View
signInPage mUsername signIn = wuiFrame hExp wHdlr
  where 
    initUsername  = maybe "" id mUsername
    focusUsername = maybe True (\_ -> False) mUsername
    (hExp,wHdlr)  = wuiWithErrorFormToHtml (wSignInData focusUsername)
                                           (initUsername,"") 
                                           signIn
                                           (wuiFrameToForm wuiFrame)
    wuiFrame hExp' wHdlr' =
      [(container `withId` "sign-in-page")
        [row
          [col [Xs 4,XsOffset 4]
            [panelDefault
              [panelBody
                [pageHeader
                  [h3 [] [signInIcon,text " Sign in to Smap"]]
                  ,hExp'
                  ,hr []
                  ,blueWuiBtn wHdlr' [text "Sign in"]
                  ,a [href "?forgot"] [text "Forgot password?"]]
              ,panelFooter
                [text "New to Smap? "
                ,a [href "?signup"] [text "Sign up &raquo;"]]]]]]]

--- Supplies a WUI form to send a new password to an user with a given email
--- address (if this email address is associated to an user account).
--- @param sendNewPassword - the controller which sends the new password
forgotPasswordPage :: (String -> Controller) -> View
forgotPasswordPage senNewPassword = wuiFrame hExp wHdlr
  where 
    (hExp,wHdlr) = wuiWithErrorFormToHtml wNewPasswordData
                                          ""
                                          senNewPassword
                                          (wuiFrameToForm wuiFrame)
    wuiFrame hExp' wHdlr' =
      [(container `withId` "forgot-password-page")
        [row
          [col [Xs 4,XsOffset 4]
            [panelDefault
              [panelBody
                [pageHeader
                  [h3 [] [passwordIcon,text " Forgot your password?"]]
                  ,hExp'
                  ,hr []
                  ,blueWuiBtn wHdlr' [text "Submit"],greyCancelBtn]
              ,panelFooter
                [text "Remember your password? "
                ,a [href "?signin"] [text "Sign in &raquo;"]]]]]]]

--- Supplies a WUI form to change the password of a user.
changePasswordForm'
  :: User
  -> ((String,String,String) -> User -> Controller)
  -> View
changePasswordForm' user updateuser =
 [panelWith 6
   [text "Change my password"]
   [label [] [passwordIcon, text "Old password:"], br []
   ,password oldref, br []
   ,label [] [passwordIcon, text "New password (at least six characters):"]
   ,br []
   ,password newref, br []
   ,label [] [passwordIcon, text "New password (type again):"], br []
   ,password new2ref, br []]
   [blueSubmitBtn chgpasswdHdlr [text "Change!"]]]
 where
  oldref,newref,new2ref free

  chgpasswdHdlr env =
    next $ updateuser (env oldref, env newref, env new2ref) user

changePasswordForm
  :: User
  -> ((String,String,String) -> User -> Controller)
  -> View
changePasswordForm user updateuser =
  renderWuiForm wChgPasswords ("","","") chgpasswdHdlr
    [h3 [] [passwordIcon,text " Change my password"]]
    []
    [text "Change!"]
    []
    []
 where
  chgpasswdHdlr passwds = updateuser passwds user


--------------------------------------------------------------------------------
-- WUI components                                                             --
--------------------------------------------------------------------------------

-- Primary WUI specifications

-- The WUI specification for creating a new user (signing up).
wSignUpData :: WuiSpec (String,String,String,String)
wSignUpData = wSmap4Tuple
  (wUsername     "Choose your user name"     True  nameHelp      nameErr
    `withCondition` isRequired)
  (wEmailAddress "Choose your email address" False emailHelp     emailErr
    `withCondition` isAValidEmailAddress)
  (wPassword     "Choose your password"      False passwd1Help passwd1Err
    `withCondition` isAtLeast6CharactersLong)
  (wPassword     "Repeat your password"      False passwd2Help passwd2Err
    `withCondition` isRequired)
    `withErrorRendering` wSignUpDataErrorRendering passwordMatchErr
    `withCondition`      passwordsDoMatch
  where
    passwordsDoMatch (_,_,password1,password2) = password1 == password2
    nameHelp = 
      "The user name is required for the authentication process and identifies"++
      " you throughout this web application."
    emailHelp = 
      "The email address is required when you forget your password."
    nameErr =
      "Please choose a user name."
    emailErr =
      "Please choose a valid email address."

isAtLeast6CharactersLong = (6<=) . length

wSignUpDataErrorRendering err render hExps =
  render hExps `addToClass` "has-error with-error"
               `addAttrs`   [tooltipToggle,title err]

passwd1Help = "The password is required for the authentication process. Choose a "++
              "password that is at least 6 characters long."
passwd2Help = "Repeat your password to make sure that you didn't made a typing error."
passwd1Err  = "Please choose a password with at least 6 characters."
passwd2Err  = "Please repeat your password."
passwordMatchErr = "Passwords do not match. Please try again."

-- The WUI specification for changing the password.
wChgPasswords :: WuiSpec (String,String,String)
wChgPasswords = wSmapTriple
  (wPassword "Old password:"                           False passwdOHelp passwdOErr
    `withCondition` isRequired)
  (wPassword "New password (at least six characters):" False passwd1Help passwd1Err
    `withCondition` isAtLeast6CharactersLong)
  (wPassword "New password (type again):"              False passwd2Help passwd2Err
    `withCondition` isRequired)
   `withErrorRendering` wSignUpDataErrorRendering passwordMatchErr
   `withCondition` passwordsDoMatch
  where
    passwdOHelp = "Your old password is required for changing the password."
    passwdOErr = "Please enter your old password."
    passwordsDoMatch (_,password1,password2) = password1 == password2


-- The WUI specification for the authentication process (signing in). Autofocus
-- on the username inout field is disabled if an initial username is given.
-- @param focusUsername - focus state on the username input field
wSignInData :: Bool -> WuiSpec (String,String)
wSignInData focusUsername = wSmapPair
  (wUsername "Enter your user name" focusUsername "" nameErr
    `withCondition` isRequired)
  (wPassword "Enter your password" False         "" passwordErr
    `withCondition` isRequired)
  where 
    nameErr = 
      "Please enter your user name."
    passwordErr =
      "Please enter your password."

-- A simple WUI specification for password requests.
wNewPasswordData :: WuiSpec String
wNewPasswordData =
  (wEmailAddress "Enter your email address" True emailHelp emailErr)
    `withCondition` isAValidEmailAddress
  where 
    emailHelp =
      "Enter the email address associated with your account. A new password w"++
      "ill be sent to you in the next few minutes."
    emailErr = 
      "Please enter a valid email address."

-- Helper WUI specifications

-- A WUI specification for a user name form field.
-- @param label - the label for the input field
-- @param focus - the autofocus state of the input field
-- @param help  - help text beneath the input field
-- @param err   - a message for the error case
wUsername :: String -> Bool -> String -> String -> WuiSpec String
wUsername label focus help err =
  wSmapString ("user","text",focus) label "User name" (help,err)

-- A WUI specification for an email address form field.
-- @param label - the label for the input field
-- @param focus - the autofocus state of the input field
-- @param help  - help text beneath the input field
-- @param err   - a message for the error case
wEmailAddress :: String -> Bool -> String -> String -> WuiSpec String
wEmailAddress label focus help err =
  wSmapString ("envelope","text",focus) label "Email address" (help,err)


-- A WUI specification for an password form field.
-- @param label - the label for the input field
-- @param focus - the autofocus state of the input field
-- @param help  - help text beneath the input field
-- @param err   - a message for the error case
wPassword :: String -> Bool -> String -> String -> WuiSpec String
wPassword label focus help err =
  wSmapString ("lock","password",focus) label "Password" (help,err)

-- Conditions

-- A predicate that checks whether the submitted string value of a WUI form
-- field represents a valid email address.
isAValidEmailAddress :: String -> Bool
isAValidEmailAddress input =
  (not $ null localPart)           &&
  (not $ null $ drop 1 domainPart) &&
  (not $ null leftDomainPart)      &&
  (not $ null $ drop 1 rightDomainPart)
  where 
    (localPart,domainPart)           = break (=='@') input
    (leftDomainPart,rightDomainPart) = break (=='.') domainPart

--------------------------------------------------------------------------------