--------------------------------------------------------------------------------
--- This module provides an interface to the comment model. Thus, it provides
--- functionality for accessing and modifying comment attributes, basic database
--- operations for storing, reading, updating and deleting comment entities
--- (CRUD operations, transactions, queries) with respect to database
--- consistency and predicates on comments. 
---
--- Further business logic concerning comments is stored in the corresponding
--- second-level controller module `CommentsController`.
---
--- (Note: The actual definition of the `Comment` datatype (and its accessors)
--- can be found in the module `Smap` which should not be modified or imported
--- for stackability reasons. Always modify or import `CommentModel` instead.)
---
--- @author Lasse Kristopher Meyer
--- @version October 2022
--------------------------------------------------------------------------------

module Model.Comment (
  Comment,CommentKey,commentText,commentDate,

  createComment,deleteComment
) where

import Data.Time
import KeyDatabase
import Model.Smap

import Model.Program

--------------------------------------------------------------------------------
-- Database CRUD operations on comment entities                               --
--------------------------------------------------------------------------------

--- Creates a new comment entity in the database.
--- @param text - the text of the comment
--- @param date - the date of the comment
--- @param user - the author of the comment
--- @param prog - the program the comment is associated to
createComment
  :: (String,CalendarTime,User,Program) -> IO (Either Comment TError)
createComment (text,date,user,prog) =
  runT $
    Model.Smap.newCommentWithUserCAuthoringKeyWithMetadataCommentingKey text
           date (userKey user) (programKey prog)

--- Deletes an existing comment entity from the database.
--- @param com - the comment entity to be deleted
deleteComment
  :: Comment -> IO (Either () TError)
deleteComment = runT . Model.Smap.deleteComment

--------------------------------------------------------------------------------
-- Predicates on comment entities                                             --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------