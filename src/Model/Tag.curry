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
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version July 2014
--------------------------------------------------------------------------------

module Model.Tag (
  Tag,TagKey,tagName,

  getAllTagsAndPopularTags,getAllPopularTags,getTagNumber,getTagByKey,
  deleteTag,leqTagName
) where

import Data.Char ( toUpper )
import Data.List (sortBy )
import KeyDatabase

import System.Models
import Model.Smap

--------------------------------------------------------------------------------
-- Database CRUD operations on tag entities                                   --
--------------------------------------------------------------------------------

--- Returs all currently persisted tag entities in the database sorted by name
--- in ascending order together with a list of these tags sorted by their
--- usage count (second component) in descending order.
getAllTagsAndPopularTags :: IO ([Tag],[(Tag,Int)])
getAllTagsAndPopularTags =
  do tags <- runQ queryAllTags
     weightedTags <- getAllPopularTagsFromTags tags
     return (sortBy (snd leqTagName) tags, weightedTags)

--- Returns a list of all tags sorted by their usage count  (second component)
--- in descending order.
getAllPopularTags :: IO [(Tag,Int)]
getAllPopularTags = runQ queryAllTags >>= getAllPopularTagsFromTags

--- Returns a list of all tags sorted by their usage count (second component)
--- in descending order where the list of all tags is provided.
getAllPopularTagsFromTags :: [Tag] -> IO [(Tag,Int)]
getAllPopularTagsFromTags tags =
  do tagns        <- runQ queryAllTaggings
     weightedTags <- return $ map (\t -> (t,length $ filter (cond t) tagns)) tags
     sortedWTags  <- return $ sortBy geq weightedTags
     return sortedWTags
  where
    cond tag            = (tagKey tag==) . taggingTagTaggingKey 
    geq (_,n1) (_,n2) = n1 >= n2

--- Returns the number of programs containing this tag.
getTagNumber :: Tag -> IO Int
getTagNumber tag = do
  taggings <- runQ queryAllTaggings
  return (length (filter ((==(tagKey tag)) . taggingTagTaggingKey) taggings))

--- Returns a tag with the given database key. `Nothing` is returned if no
--- such tag exists.
--- @param tagkey - the key of the tag
getTagByKey :: TagKey -> IO (Maybe Tag)
getTagByKey tagkey = runT (getTag tagkey) >>= return . either Just (const Nothing)

--- Deletes a tag.
deleteTag :: Tag -> IO (Either () TError)
deleteTag tag = runT (Model.Smap.deleteTag tag)

--------------------------------------------------------------------------------
-- Predicates on tag entities                                                 --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Tag sortings                                                               --
--------------------------------------------------------------------------------

--- Compares two tags regarding to their names.
leqTagName :: Sorting Tag
leqTagName =
  (LEQ,
   \tag1 tag2 -> map toUpper (tagName tag1) <= map toUpper (tagName tag2))

--------------------------------------------------------------------------------