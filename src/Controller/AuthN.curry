--------------------------------------------------------------------------------
--- This module provides the handling of all functionality related to user
--- authentication (sign in, sign up, assignment of new passwords).
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version July 2020
--------------------------------------------------------------------------------

module Controller.AuthN (
  authNController,
  signUpForm, signInForm, forgotPasswordForm, changePasswordForm
) where

import Data.Maybe      ( isNothing )

import HTML.Session
import System.Mail     ( sendMail )
import WUI

import Config.Smap        ( smapEmail )
import System.Alerts
import System.Authentication
import System.Authorization
import System.AuthorizedOperations
import View.AuthN
import System.Controllers
import Controller.Static  ( showLandingPage )
import View.Static
import System.Url
import Model.User
import Controller.Users
import System.SmapHtml
import System.Views

--------------------------------------------------------------------------------
-- Delegation controller                                                      --
--------------------------------------------------------------------------------

--- The main authentication controller which delegates tasks to other
--- controllers in this module depending on the URL path.
--- @param url - the current URL
authNController :: Url -> Controller
authNController url@(path,_) =
  case path of
    ["signup"]  -> showBlankSignUpPage
    ["signin"]  -> showBlankSignInPage
    ["signout"] -> doSignOut
    ["forgot"]  -> showForgotPasswordPage
    ["passwd"]  -> showChangePasswdForm
    _           -> showInvalidUrlErrorPage url

--------------------------------------------------------------------------------
-- Signing up                                                                 --
--------------------------------------------------------------------------------

showBlankSignUpPage :: Controller
showBlankSignUpPage = showSignUpPage ("","","","")

-- Returns a controller that displays the sign up page where new users are
-- created.
showSignUpPage :: (String,String,String,String) -> Controller
showSignUpPage initCreationData =
  checkAuthorization (authNOperation SignUp) $ \_ -> do
    setWuiStore signUpStore initCreationData
    return [formElem signUpForm]

--- A WUI form to sign up.
--- The default values for the fields are stored in 'signUpStore'.
signUpForm :: HtmlFormDef (WuiStore (String,String,String,String))
signUpForm =
  wui2FormDef "Controller.AuthN.signUpForm" signUpStore
   wSignUpData
   (\newuserdata ->
     checkAuthorization (authNOperation SignUp) $ \_ ->
       doCreateUser (showSignUpPage       , Just nameNotUniqueErrAlert )
                    (showSignUpPage       , Just emailNotUniqueErrAlert)
                    (showSignInPage . Just, Just signUpSucceededAlert  )
                    newuserdata)
   (renderWui
      [h3 [] [signUpIcon, text " Sign up for Smap"]]
      []
      "Sign up"
      []
      [text "Already have an account? "
      ,a [href "?signin"] [text "Sign in &raquo;"]])

 where
  nameNotUniqueErrAlert = 
    ErrorAlert $ "This user name is already taken. Please try another one."
  emailNotUniqueErrAlert = 
    ErrorAlert $ "This email address already exists. Please try another one."
  signUpSucceededAlert = 
    SuccessAlert $
      "You can now sign in to Smap with your user name and password!"

--- The data stored for executing the "signUp" WUI form.
signUpStore :: SessionStore (WuiStore (String,String,String,String))
signUpStore = sessionStore "signUpStore"
                      
--------------------------------------------------------------------------------
-- Signing in                                                                 --
--------------------------------------------------------------------------------

showBlankSignInPage :: Controller
showBlankSignInPage = showSignInPage Nothing

-- Returns a controller that displays the sign in page where users can
-- authenticate themselves.
showSignInPage :: Maybe User -> Controller
showSignInPage muser = 
  checkAuthorization (authNOperation SignIn) $ \_ -> do
    let initUsername  = maybe "" userName muser
    setParWuiStore signInStore (isNothing muser) (initUsername,"")
    return [formElem signInForm]

--- A WUI form to sign in.
--- The default values for the fields are stored in 'signInStore'.
signInForm :: HtmlFormDef (Bool, WuiStore (String,String))
signInForm =
  pwui2FormDef "Controller.AuthN.signInForm" signInStore
   (\focusUsername -> wSignInData focusUsername)
   (\_ logindata ->
     checkAuthorization (authNOperation SignIn) $ \_ ->
       doSignIn logindata)
   (\_ -> signInRenderer)

--- The data stored for executing the "signIn" WUI form.
signInStore :: SessionStore (Bool, WuiStore (String,String))
signInStore = sessionStore "signInStore"

-- Performs the sign in process and stores the authentication data in the
-- current session (see module `Authentication`). An error alert is displayed if
-- no user was found for the given sign in data.
-- @param signInData - username and password
doSignIn :: (String,String) -> Controller
doSignIn (userName,password) = 
  checkAuthorization (authNOperation SignIn) $ \_ ->
  do userHash <- getUserHash userName password
     mUser    <- getUserByNameWith userName (Nothing,Just userHash)
     maybe (do setAlert signInFailedAlert
               showSignInPage mUser)
           (\user -> do signInToSession (userName,userIsAdmin user)
                        return $ landingPage)
           mUser
  where
    signInFailedAlert =
     ErrorAlert $ "<strong>Oh snap!</strong> Your attempt to sign in failed "++
      "due to an unknown user name and password combination. Try again!"

-- Signs out an user and deletes the associated authentication data from the
-- session (see module `Authentication`).
doSignOut :: Controller
doSignOut =
  checkAuthorization (authNOperation SignOut) $ \_ -> 
  do signOutFromSession
     return landingPage

--------------------------------------------------------------------------------
-- Assigning new passwords                                                    --
--------------------------------------------------------------------------------

-- Returns a controller that displays a page where users can request a new
-- password (when they forgot their old one) by entering the email address
-- associated with their account.
showForgotPasswordPage :: Controller
showForgotPasswordPage = 
  checkAuthorization (authNOperation SendNewPassword) $ \_ -> do
    setWuiStore forgotPWStore ""
    return [formElem forgotPasswordForm]

--- A WUI form to request a new password.
--- The default values for the fields are stored in 'forgotPWStore'.
forgotPasswordForm :: HtmlFormDef (WuiStore String)
forgotPasswordForm =
  wui2FormDef "Controller.AuthN.forgotPasswordForm" forgotPWStore
   wNewPasswordData
   (\emailaddr ->
     checkAuthorization (authNOperation SendNewPassword) $ \_ ->
       doSendNewPassword emailaddr)
   forgotPasswordRenderer

--- The data stored for executing the "forgotPassword" WUI form.
forgotPWStore :: SessionStore (WuiStore String)
forgotPWStore = sessionStore "forgotPWStore"

-- Takes an email address and sends a new password if the email address is 
-- associated to an actual user account. Otherwise, an error alert is displayed.
-- @param userEmail - the entered email address
doSendNewPassword :: String -> Controller
doSendNewPassword userEmail =
  checkAuthorization (authNOperation SendNewPassword) $ \_ -> do
    mUser <- getUserByEmail userEmail
    maybe (do setAlert emailNotFoundErrAlert
              showForgotPasswordPage)
          (\user -> do
              newPassword <- randomPassword 6
              userHash    <- getUserHash (userName user) newPassword
              tResult     <- updateUser $ setUserHash user userHash
              either (\_ -> do sendMail smapEmail userEmail subject
                                        (content user newPassword)
                               setAlert sendingInProcessAlert
                               showSignInPage Nothing)
                     (showTransactionErrorPage updateUserFailedErr)
                     tResult)
          mUser
  where
    emailNotFoundErrAlert = 
     ErrorAlert $ "This email address is not associated with an user account."++
      " Please try again."
    sendingInProcessAlert =
     SuccessAlert $ "An email with a new password will be sent to your email "++
      "address in the next few minutes."
    subject =
      "Your new password for Smap!"
    content user newPassword =
      "Hello "++userName user++"!\n\nForgot your password? No problem. We res"++
      "tored your password. Here it is:\n\n"++newPassword++"\n\nYou can now u"++
      "se this password to sign in to Smap (and to change it there again!)."
    updateUserFailedErr =
      "The new password could not be saved due to an unexpected internal error."

--------------------------------------------------------------------------------
-- Change password                                                            --
--------------------------------------------------------------------------------

--- Returns a controller to change the user's password.
showChangePasswdForm :: Controller
showChangePasswdForm =
  checkAuthorization (authNOperation ChangePasswd) $ \authzData ->
  do mUser <- maybe (return Nothing) getUserByName
                    (getUsernameFromAuthZData authzData)
     maybe (showStdErrorPage userNotFoundErr)
           (\user -> do setParWuiStore changePWStore user ("","","")
                        return [formElem changePasswordForm])
           mUser
 where
  userNotFoundErr = "Unexpectedly, no user was found with the signed-in user."

--- A WUI form to change the user's password.
--- The default values for the fields are stored in 'changePWStore'.
changePasswordForm :: HtmlFormDef (User, WuiStore (String,String,String))
changePasswordForm =
  pwui2FormDef "Controller.AuthN.changePasswordForm" changePWStore
   (\_ -> wChgPasswords)
   (\user passworddata ->
     checkAuthorization (authNOperation ChangePasswd) $ \_ ->
       doChangePasswdCtrl passworddata user)
   (\_ -> renderWui [h3 [] [passwordIcon, text " Change my password"]]
                    [] "Change!" [] [])

--- The data stored for executing the "changePassword" WUI form.
changePWStore :: SessionStore (User, WuiStore (String,String,String))
changePWStore = sessionStore "changePWStore"
                      
doChangePasswdCtrl :: (String,String,String) -> User -> Controller
doChangePasswdCtrl (oldpass,newpass,newpass2) user = do
    oldpasshash <- getUserHash (userName user) oldpass
    if oldpasshash /= userHash user
     then setAlert wrongOldPasswordAlert >> showLandingPage
     else
      if newpass /= newpass2
      then setAlert wrongPasswordRepeatAlert >> showLandingPage
      else
       if length newpass < 6
       then setAlert passwordTooShortAlert >> showLandingPage
       else do
        newpasshash <- getUserHash (userName user) newpass
        doUpdateUser
          (const showLandingPage, Just nameNotUniqueErrAlert )
          (const showLandingPage, Just emailNotUniqueErrAlert)
          (const showLandingPage, Just changePasswdSucceededAlert)
          (setUserHash user newpasshash)
 where
  nameNotUniqueErrAlert = 
    ErrorAlert "This user name is already taken. Please try another one."
  emailNotUniqueErrAlert = 
    ErrorAlert "This email address already exists. Please try another one."
  changePasswdSucceededAlert = 
    SuccessAlert "Your password has been changed!"

  wrongOldPasswordAlert = 
    ErrorAlert "Old password is wrong. Nothing changed!"
  wrongPasswordRepeatAlert = 
    ErrorAlert "The new passwords are different. Nothing changed!"
  passwordTooShortAlert =
    ErrorAlert "Please choose a new password with at least 6 characters."

--------------------------------------------------------------------------------
