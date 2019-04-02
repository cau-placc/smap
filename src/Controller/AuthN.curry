--------------------------------------------------------------------------------
--- This module provides the handling of all functionality related to user
--- authentication (sign in, sign up, assignment of new passwords).
---
--- @author Lasse Kristopher Meyer
--- @version November 2018
--------------------------------------------------------------------------------

module Controller.AuthN (
  authNController
) where

import Mail

import System.Alerts
import System.Authentication
import System.Authorization
import System.AuthorizedOperations
import View.AuthN
import System.Controllers
import Controller.Static(showLandingPage)
import View.Static
import System.Url
import Model.User
import Controller.Users

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
  checkAuthorization (authNOperation SignUp) $ \_ ->
  return $ signUpPage initCreationData $ 
                      doCreateUser 
                        (showSignUpPage       ,Just nameNotUniqueErrAlert )
                        (showSignUpPage       ,Just emailNotUniqueErrAlert)
                        (showSignInPage . Just,Just signUpSucceededAlert  )
  where
    nameNotUniqueErrAlert = 
      ErrorAlert $ "This user name is already taken. Please try another one."
    emailNotUniqueErrAlert = 
      ErrorAlert $ "This email address already exists. Please try another one."
    signUpSucceededAlert = 
      SuccessAlert $ "You can now sign in to Smap with your user name and "++
      "password!"

--------------------------------------------------------------------------------
-- Signing in                                                                 --
--------------------------------------------------------------------------------

showBlankSignInPage :: Controller
showBlankSignInPage = showSignInPage Nothing

-- Returns a controller that displays the sign in page where users can
-- authenticate themselves.
showSignInPage :: Maybe User -> Controller
showSignInPage mUser =
  checkAuthorization (authNOperation SignIn) $ \_ ->
  return $ signInPage (maybe Nothing (Just . userName) mUser) doSignIn

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
               return $ signInPage (Just userName) doSignIn)
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
  checkAuthorization (authNOperation SendNewPassword) $ \_ ->
  return $ forgotPasswordPage doSendNewPassword

-- Takes an email address and sends a new password if the email address is 
-- associated to an actual user account. Otherwise, an error alert is displayed.
-- @param userEmail - the entered email address
doSendNewPassword :: String -> Controller
doSendNewPassword userEmail =
  checkAuthorization (authNOperation SendNewPassword) $ \_ ->
  do mUser <- getUserByEmail userEmail
     maybe (do setAlert emailNotFoundErrAlert
               return $ forgotPasswordPage doSendNewPassword)
           (\user -> do newPassword <- randomPassword 6
                        userHash    <- getUserHash (userName user) newPassword
                        tResult     <- updateUser $ setUserHash user userHash
                        either (\_ -> do sendMail "Smap" 
                                                  userEmail
                                                  subject
                                                  (content user newPassword)
                                         setAlert sendingInProcessAlert
                                         return $ signInPage Nothing 
                                                              doSignIn)
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
showChangePasswdForm =
  checkAuthorization (authNOperation ChangePasswd) $ \authzData ->
  do mUser <- maybe (return Nothing) getUserByName
                    (getUsernameFromAuthZData authzData)
     maybe (showStdErrorPage userNotFoundErr)
           (\user -> return $ changePasswordForm user doChangePasswdCtrl)
           mUser
 where
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
  userNotFoundErr = "Unexpectedly, no user was found with the signed-in user."

--------------------------------------------------------------------------------
