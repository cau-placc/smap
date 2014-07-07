--------------------------------------------------------------------------------
--- This module provides an interface to the tag model. Thus, it provides
--- functionality for accessing and modifying tag attributes, basic database
--- operations for storing, reading, updating and deleting tag entities (CRUD
--- operations, transactions, queries) with respect to database consistency and
--- predicates on tags. 
---
--- (Note: The actual definition of the `Tag` datatype (and its accessors) can
--- be found in the module `Smap` which should not be modified or imported for
--- stackability reasons. Always modify or import `TagModel` instead.)
---
--- @author Lasse Kristopher Meyer
--- @version February 2014
--------------------------------------------------------------------------------

module TagModel (
  Tag,TagKey,tagName,

  getAllTags,getAllPopularTags,
  leqTagName
) where

import KeyDatabase
import Sort

import Models
import Smap

--------------------------------------------------------------------------------
-- Database CRUD operations on tag entities                                   --
--------------------------------------------------------------------------------

--- Returs all currently persisted tag entities in the database sorted by name
--- in ascending order.
getAllTags :: IO [Tag]
getAllTags =
  do tags <- runQ queryAllTags
     return $ mergeSort leqTagNameFunc tags
  where leqTagNameFunc = snd leqTagName

--- Returns a list of all tags sorted by their usage count in descending order.
getAllPopularTags :: IO [Tag]
getAllPopularTags =
  do tags         <- runQ queryAllTags
     tagns        <- runQ queryAllTaggings
     weightedTags <- return $ map (\t -> (t,length$filter (cond t) tagns)) tags
     sortedWTags  <- return $ quickSort geq weightedTags
     return $ map fst sortedWTags
  where
    cond tag            = (tagKey tag==) . taggingTagTaggingKey 
    geq (_,n1) (_,n2) = n1 >= n2

--------------------------------------------------------------------------------
-- Predicates on tag entities                                                 --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Tag sortings                                                               --
--------------------------------------------------------------------------------

--- Compares two tags regarding to their names.
leqTagName :: Sorting Tag
leqTagName = (LEQ,\tag1 tag2 ->
  (tagName tag1) `leqStringIgnoreCase`
  (tagName tag2))

--------------------------------------------------------------------------------