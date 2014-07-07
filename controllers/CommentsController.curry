--------------------------------------------------------------------------------
--- Second-level entity controller for the general handling of comment entities.
--- Provides CRUD functionality for the use in top-level application component
--- controllers and covers business logic like input validation (if not done in
--- the view) and error handling (setting alerts, returning error pages).
---
--- All controllers in this module take a pair consisting of a controller and a
--- possibly given alert for the case of a successful operation where the
--- controller usually is applicable on the result of this operation.
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module CommentsController (
  doCreateComment,doCreateCommentForCurrentUser
) where

import Time

import CommentModel
import ProgramModel
import UserModel

import Alerts
import Authentication
import Controllers

--------------------------------------------------------------------------------
-- CRUD operations on comment entities                                        --
--------------------------------------------------------------------------------

--- Returns a controller that persists a new comment entity to the database.
--- @param successData  - next controller and possibly given alert for the case
---   of a successful comment creation
--- @param creationData - required data for the comment creation (text, user and
---   associated program)
doCreateComment
  :: (Comment -> Controller,Maybe Alert)
  -> (String,User,Program)
  -> Controller
doCreateComment (successCtrl,mSuccessAlert) (text,user,prog) =
  do date     <- getLocalTime
     transRes <- CommentModel.createComment (text,date,user,prog)
     either (\com -> do maybeSetAlert mSuccessAlert
                        successCtrl com)
            (showTransactionErrorPage commentCreationFailedErr)
            transRes
  where
    commentCreationFailedErr =
      "The comment creation failed due to an unexpected internal error. See t"++
      "he internal error message for additional details."

--- Returns a controller that persists a new comment entity to the database
--- with the currently authenticated user as its author. If the current user is
--- not authenticated or does not exist, an appropriate alert can be set and a
--- corresponding controller will be called.
--- @param noCurrUserFoundData - next controller and possibly given alert for
---   the case that the current user is not signed in or does not exist
--- @param successData         - next controller and possibly given alert for
---   the case of an successful comment creation
--- @param creationData        - required data for the comment creation (text
---   and associated program)
doCreateCommentForCurrentUser
  :: ((String,Program) -> Controller,Maybe Alert)
  -> (Comment          -> Controller,Maybe Alert)
  -> (String,Program)
  -> Controller
doCreateCommentForCurrentUser (noCurrUserFoundCtrl,mNoCurrUserFoundErrAlert)
                              successData
                              creationData@(text,prog) =
  do mUser <- getCurrentUser
     maybe (do maybeSetAlert mNoCurrUserFoundErrAlert
               noCurrUserFoundCtrl creationData)
           (\user -> doCreateComment successData (text,user,prog))
           mUser

--------------------------------------------------------------------------------