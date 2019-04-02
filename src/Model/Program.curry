--------------------------------------------------------------------------------
--- This module provides an interface to the program model. Thus, it provides
--- functionality for accessing and modifying programs attributes, basic
--- CRUD functionality for storing, reading, updating and deleting programs
--- with with respect to database consistency and predicates on programs. In
--- addition, this module provides the abstract data type `ProgramQuery` which
--- encapsulates data that is needed for complex program searches 
---
--- Further business logic concerning comments is stored in the corresponding
--- second-level controller module `ProgramsController`.
---
--- (Note: Programs are composed abstract data types that contain multiple
--- associated entities from the ERD specification. Not all of these entities
--- are freely accessible via the accessors in this module.)
---
--- @author Lasse Kristopher Meyer (with changes by Michael Hanus)
--- @version November 2018
--------------------------------------------------------------------------------

module Model.Program (
  Language,languageName,
  User,userName,
  Version,versionNumber,versionSourceCode,versionMessage,versionDate,
  Tag,tagName,
  Comment,commentText,commentDate,

  Program,ProgramKey,programKey,setProgramTitle,programTitle,
  programDescription,setProgramDescription,
  setProgramIsVisible,programIsVisible,programImplLang,programAuthor,
  programVersions,programTags,programComments,programFavoriters,
  programFirstVersion,programLatestVersion,programLatestComment,
  showProgramKey,readProgramKey,readVersionNumber,
  readProgramKeyAndVersionNumber,
  createProgram,createProgramVersion,
  updateProgramMetadata,updateProgramMetadataWithTags,
  deleteProgram,
  getProgramByKey,getAllProgramsWith,

  ProgramQuery,defaultProgramQuery,
  withKey,withKeyword,withInfixIgnoreCaseKeywordInTitle,
  withInfixIgnoreCaseKeywordInDescr,withExactIgnoreCaseKeywordInTags,
  withIsVisibleOnly,withExactIgnoreCaseImplLangName,withExactAuthorName,
  withAddExactFavoriterName,withSorting,withOrdering,
  leqProgramTitle,leqProgramImplLangName,leqProgramFirstVersionDate,
  leqProgramLatestVersionDate,leqProgramFavoriterCount
) where

import Char
import KeyDatabase
import List        (last, (\\), isInfixOf)
import Maybe
import ReadNumeric
import Sort        ( leqStringIgnoreCase, sortBy )
import Time

import System.Models
import Model.Smap
import System.Url

--------------------------------------------------------------------------------
-- Program data type                                                          --
--------------------------------------------------------------------------------

--- The central abstract data type of this application. Programs contain
--- all information about their associated entities such as
--- - the program metadata
--- - the implementation language
--- - the author
--- - all version
--- - all tags of this program
--- - all comment of this program
--- - all users who favorites the program
--- Since this information is needed at once in most instances, program objects
--- encapsulate all of the corresponding entities.
data Program = Program Metadata Language User [Version] [Tag] [Comment] [User]
  deriving Eq

--- The key of a program is identical to the key of its associated `Metadata`
--- entity.
type ProgramKey = MetadataKey

--------------------------------------------------------------------------------
-- Program accessors                                                          --
--------------------------------------------------------------------------------

-- General accessors

-- Returns the metadata object of a program (for internal use only).
programMetadata :: Program -> Metadata
programMetadata (Program mdata _ _ _ _ _ _) = mdata

--- Returns the program key.
programKey :: Program -> ProgramKey
programKey (Program mdata _ _ _ _ _ _) = metadataKey mdata

--- Returns the program title.
programTitle :: Program -> String
programTitle (Program mdata _ _ _ _ _ _) = metadataTitle mdata

--- Sets the title of a given program. 
--- @param title - the new title
--- @param prog  - the old program 
setProgramTitle :: String -> Program -> Program
setProgramTitle title (Program m l a vs ts cs fs) =
  Program (setMetadataTitle m title) l a vs ts cs fs

--- Returns the program description.
programDescription :: Program -> String
programDescription (Program mdata _ _ _ _ _ _) = metadataDescription mdata

--- Sets the description of a given program. 
--- @param descr - the new description
--- @param prog  - the old program 
setProgramDescription :: String -> Program -> Program
setProgramDescription descr (Program m l a vs ts cs fs) =
  Program (setMetadataDescription m descr) l a vs ts cs fs

--- Sets the visibility state of a given program. 
--- @param isVisisble - the new visibility state (if set to `True` the program
---   is marked as public and can be viewed by all users; if set to `False` the
---   program is only accessible by its author.
--- @param prog       - the program whose visibilty state will be changed
setProgramIsVisible :: Bool -> Program -> Program
setProgramIsVisible isVisible (Program m l a vs ts cs fs) =
  Program (setMetadataIsVisible m isVisible) l a vs ts cs fs

--- Returns the visibility state of a program.
programIsVisible :: Program -> Bool
programIsVisible (Program mdata _ _ _ _ _ _) = metadataIsVisible mdata

--- Returns the program implementation language of a program.
programImplLang :: Program -> Language
programImplLang (Program _ lang _ _ _ _ _) = lang

--- Returns the author of a program.
programAuthor :: Program -> User
programAuthor (Program _ _ author _ _ _ _) = author

--- Returns all versions of a program (ordered by creation date, ascending).
programVersions :: Program -> [Version]
programVersions (Program _ _ _ vers _ _ _) = vers

--- Returns all tags of a program (ordered by their name, ascending).
programTags :: Program -> [Tag]
programTags (Program _ _ _ _ tags _ _) = tags

--- Returns all comments of a program (ordered by creation date, ascending).
programComments :: Program -> [Comment]
programComments (Program _ _ _ _ _ coms _) = coms

--- Returns all users who favorited the program (ordered by favoriting date,
--- ascending).
programFavoriters :: Program -> [User]
programFavoriters (Program _ _ _ _ _ _ favers) = favers

-- Quick accessors

--- Returns the first version of a program.
programFirstVersion :: Program -> Version
programFirstVersion = head . programVersions

--- Returns the latest version of a program.
programLatestVersion :: Program -> Version
programLatestVersion = last . programVersions

--- Returns the latest comment of a program. Returns `Nothing` if the program
--- has no comments yet.
programLatestComment :: Program -> Maybe Comment
programLatestComment prog = if null coms then Nothing else Just $ last coms
  where
    coms = programComments prog

--------------------------------------------------------------------------------
-- Showing and reading keys and version numbers                               --
--------------------------------------------------------------------------------

--- Returns the string representation of the program key.
showProgramKey :: Program -> String
showProgramKey = snd . break isDigit . showMetadataKey . programMetadata

--- Transforms a string into a key of a program. `Nothing` is returned if the
--- string does not represent a reasonable key.
--- @param str - the string that possibly represents a program key
readProgramKey :: String -> Maybe ProgramKey
readProgramKey = readMetadataKey . ("Metadata"++)

--- Reads a version number from a given string. `Nothing` is returned if the
--- string does not contain a reasonable version number.
--- @param str - the string that possibly contains a version number
readVersionNumber :: String -> Maybe Int
readVersionNumber = maybe Nothing (Just . fst) . readNat 

--- Reads a program key and a version number from a pair of strings. `Nothing`
--- is returned if one of the strings is not a valid value.
--- @param progKeyAndVersNumStrs - strings that possibly represent a program key
---   and a version number
readProgramKeyAndVersionNumber :: (String,String) -> Maybe (ProgramKey,Int)
readProgramKeyAndVersionNumber (mProgKeyStr,mVersNumStr) =
  maybe Nothing
        (\progKey -> maybe Nothing
                           (\versNum -> Just (progKey,versNum))
                           (readVersionNumber mVersNumStr))
        (readProgramKey mProgKeyStr) 

--------------------------------------------------------------------------------
-- Database CRUD operations on programs and program versions                  --
--------------------------------------------------------------------------------

-- Creating, updating and deleting programs

--- Creates a new program and persists its components to the database.
--- @param progData - title, description, visibilty, implementation language,
---   author, code, date and tag names
createProgram
  :: (String,String,Bool,Language,User,String,CalendarTime,[String])
  -> IO (Either Program TError)
createProgram (title,descr,visible,lang,author,code,date,tagNames) =
  do tagsOld <- getAllTagsWithNameIn
     runT $ createMetadataT
            |>>= (\mdata -> createVersionT (1,"Program created.",mdata)
                 |>>= (\version -> returnT (mdata,[version])))
            |>>= (\(mdata,vers) -> mapT newTag (getNewTagNames tagsOld)
                 |>>= (\tagsNew -> let tags = tagsNew++tagsOld
                                    in mapT_ (addTaggingT mdata) tags
                      |>> (returnT$Program mdata lang author vers tags [] [])))
  where 
    createMetadataT = newMetadataWithLanguageImplLangKeyWithUserAuthoringKey
      title descr visible (languageKey lang) (userKey author)
    createVersionT (num,msg,mdata) = newVersionWithMetadataVersioningKey
      num code msg date (metadataKey mdata)
    addTaggingT mdata tag =
      newTagging (metadataKey mdata) (tagKey tag)
    getAllTagsWithNameIn =
      runQ $ queryCondTag (\tag -> tagName tag `elem` tagNames)
    getNewTagNames = (tagNames\\) . map tagName

--- Persists a new program version entity to the database.
--- @param versData - version number, code, message, data and associated program
createProgramVersion
  :: (Int,String,String,CalendarTime,Program) -> IO (Either Version TError)
createProgramVersion (num,code,msg,date,prog) =
  runT $ newVersionWithMetadataVersioningKey num code msg date (programKey prog)

--- Updates metadata of an existing program.
--- @param prog - the modification of an existing program
updateProgramMetadata :: Program -> IO (Either () TError)
updateProgramMetadata = runT . updateMetadata . programMetadata

--- Updates metadata and tags of an existing program.
--- @param tagnames - the names of the tags for the updated program
--- @param prog - the modification of an existing program
updateProgramMetadataWithTags :: [String] -> Program -> IO (Either () TError)
updateProgramMetadataWithTags tagnames prog = runT $
  (if null newtagnames
   then returnT []
   else getDB (queryCondTag ((`elem` newtagnames) . tagName)))
  |>>= \existingnewtags ->
       mapT newTag (newtagnames \\ map tagName existingnewtags)
  |>>= \newtags -> mapT_ addTaggingT (newtags++existingnewtags)
  |>> mapT_ delTaggingT tagsToDelete
  |>> updateMetadata (programMetadata prog)
 where 
  mdata        = programMetadata prog
  oldtags      = programTags prog
  oldtagnames  = map tagName oldtags
  tagsToDelete = filter ((`notElem` tagnames) . tagName) oldtags
  newtagnames = filter (`notElem` oldtagnames) tagnames
  addTaggingT tag = newTagging (metadataKey mdata) (tagKey tag)
  delTaggingT tag = deleteTagging (metadataKey mdata) (tagKey tag)

--- Deletes an existing program from the database. This includes the deletion of
--- the program metadata, all associated versions, all associated taggings, all
--- associated comments and all associated favoritings.
--- @param prog - the program to be deleted
deleteProgram :: Program -> IO (Either () TError)
deleteProgram prog =
  runT $ (getDB $ queryCondFavoriting cond1)
         |>>= (mapT_ ((flip deleteFavoriting) mdataKey) . userKeys)
         |>>  ((getDB $ queryCondComment cond2)
              |>>= (mapT_ deleteComment))
         |>>  (getMetadataTags mdata
              |>>= (mapT_ (deleteTagging mdataKey) . tagKeys))
         |>>  ((getDB $ queryCondVersion cond3)
              |>>= (mapT_ deleteVersion))
         |>>  deleteMetadata mdata
  where
    mdata    = programMetadata prog
    mdataKey = metadataKey mdata
    userKeys = map favoritingUserFavoritingKey
    tagKeys  = map tagKey
    cond1    = (mdataKey==) . favoritingMetadataFavoritingKey
    cond2    = (mdataKey==) . commentMetadataCommentingKey
    cond3    = (mdataKey==) . versionMetadataVersioningKey

-- Reading programs from the database

--- Returns a program with the given database key. `Nothing` is returned if no
--- such program exists.
--- @param progKey - the key of the program
getProgramByKey :: ProgramKey -> IO (Maybe Program)
getProgramByKey progKey =
  do prog <- runProgramQuery $ withKey progKey 
                             $ withIsVisibleOnly False 
                             $ defaultProgramQuery
     return $ listToMaybe prog

--- Returns all programs that satisfy the given program query.
--- @param progQuery - specification of search settings as a program query
getAllProgramsWith :: ProgramQuery -> IO [Program]
getAllProgramsWith = runProgramQuery

--------------------------------------------------------------------------------
-- Program queries                                                            --
--------------------------------------------------------------------------------

-- Data type

--- Program queries encapsulate all data that is needed for complex program
--- searches. This includes constraints to programs, filter settings and the
--- sorting and ordering of the result list. The actual implementation of the
--- search should be widely hidden and the interface to program queries should
--- be independent from it. Thus, program queries are only accessible via their
--- default constructor and accessors (see below).
data ProgramQuery = ProgramQuery 
  (Maybe ProgramKey) -- program key
  String             -- keyword
  (Bool,Bool,Bool)   -- targets (title, descr., tags)
  Bool               -- only visible programs?
  String             -- implementation lang. name
  String             -- name of the author
  [String]           -- list of favoriter names
  (Sorting Program)  -- a program sorting
  OrderingType       -- the results ordering

-- Default program query and accessors

--- The default program query. This function should always be used as a entry
--- point for the construction of new program queries.
defaultProgramQuery :: ProgramQuery
defaultProgramQuery = ProgramQuery Nothing "" (True,True,True) True "" "" []
                                   leqProgramFirstVersionDate Descending

--- Adds a constraint to a program query. Returns only programs with the given
--- key.
withKey :: ProgramKey -> ProgramQuery -> ProgramQuery
withKey key (ProgramQuery _ kw ts v l a fs s o) =
  ProgramQuery (Just key) kw ts v l a fs s o

--- Adds a constraint to a program query. Returns only programs with the given
--- keyword in the program title, description or tags.
withKeyword :: String -> ProgramQuery -> ProgramQuery
withKeyword keyword (ProgramQuery mK _ ts v l a fs s o) =
  ProgramQuery mK keyword ts v l a fs s o

--- ...
withInfixIgnoreCaseKeywordInTitle :: Bool -> ProgramQuery -> ProgramQuery
withInfixIgnoreCaseKeywordInTitle t (ProgramQuery mK kw (_,d,ts) v l a fs s o) =
  ProgramQuery mK kw (t,d,ts) v l a fs s o

--- ...
withInfixIgnoreCaseKeywordInDescr :: Bool -> ProgramQuery -> ProgramQuery
withInfixIgnoreCaseKeywordInDescr d (ProgramQuery mK kw (t,_,ts) v l a fs s o) =
  ProgramQuery mK kw (t,d,ts) v l a fs s o

--- ...
withExactIgnoreCaseKeywordInTags :: Bool -> ProgramQuery -> ProgramQuery
withExactIgnoreCaseKeywordInTags ts (ProgramQuery mK kw (t,d,_) v l a fs s o) =
  ProgramQuery mK kw (t,d,ts) v l a fs s o

--- ...
withIsVisibleOnly :: Bool -> ProgramQuery -> ProgramQuery
withIsVisibleOnly isVisible (ProgramQuery mK kw ts _ l a fs s o) =
  ProgramQuery mK kw ts isVisible l a fs s o

--- ...
withExactIgnoreCaseImplLangName :: String -> ProgramQuery -> ProgramQuery
withExactIgnoreCaseImplLangName langName (ProgramQuery mK kw ts v _ a fs s o) = 
  ProgramQuery mK kw ts v langName a fs s o

--- ...
withExactAuthorName :: String -> ProgramQuery -> ProgramQuery
withExactAuthorName authorName (ProgramQuery mK kw ts v l _ fs s o) =
  ProgramQuery mK kw ts v l authorName fs s o

--- ...
withAddExactFavoriterName :: String -> ProgramQuery -> ProgramQuery
withAddExactFavoriterName faverName (ProgramQuery mK kw ts v l a fs s o) =
  ProgramQuery mK kw ts v l a (faverName:fs) s o

--- ...
withSorting :: Sorting Program -> ProgramQuery -> ProgramQuery
withSorting sorting (ProgramQuery mK kw ts v l a fs _ o) =
  ProgramQuery mK kw ts v l a fs sorting o

--- ...
withOrdering :: OrderingType -> ProgramQuery -> ProgramQuery
withOrdering orderingType (ProgramQuery mK kw ts v l a fs s _) =
  ProgramQuery mK kw ts v l a fs s orderingType

-- Search implementation

-- Runs a program query and returns the list of results.
-- @param progQuery - the program query specifying the search settings
runProgramQuery :: ProgramQuery -> IO [Program]
runProgramQuery progQuery@(ProgramQuery _ _ _ _ _ _ _ (sType,sFunc) oType) =
  do mdatas   <- runQ queryAllMetadatas
     progs    <- metadatasToPrograms mdatas
     filter   <- getFilter progQuery
     ordering <- getOrdering sType oType
     return $ sortBy (ordering sFunc) $ filter progs

-- Takes a list of metadata entities and returns a list of the corresponding
-- programs as an I/O action.
-- @param mdatas - the list of metadata entities
metadatasToPrograms :: [Metadata] -> IO [Program]
metadatasToPrograms mdatas =
  do langs <- runQ queryAllLanguages
     users <- runQ queryAllUsers
     vers  <- runQ queryAllVersions
     tagns <- runQ queryAllTaggings
     tags  <- runQ queryAllTags
     coms  <- runQ queryAllComments
     favs  <- runQ queryAllFavoritings
     return $ map (\mdata -> 
       let mdataKey = metadataKey mdata
        in Program mdata
                   (getLang   mdata    langs     ) 
                   (getAuthor mdata    users     )
                   (getVers   mdataKey vers      )
                   (getTags   mdataKey tagns tags)
                   (getComs   mdataKey coms      )
                   (getFavers mdataKey favs users)) 
       mdatas
  where
    getLang mdata = 
      head . filter ((metadataLanguageImplLangKey mdata==).languageKey)
    getAuthor mdata =
      head . filter ((metadataUserAuthoringKey mdata==).userKey)
    getVers mdataKey =
      filter ((mdataKey==).versionMetadataVersioningKey)
    getTags mdataKey tagns tags =
      let mTgns    = filter ((mdataKey==).taggingMetadataTaggingKey) tagns
          mTgnKeys = map taggingTagTaggingKey mTgns
       in filter (\tag -> (tagKey tag)`elem`mTgnKeys) tags
    getComs mdataKey =
      filter ((mdataKey==).commentMetadataCommentingKey)
    getFavers mdataKey favs users =
      let mFavs    = filter ((mdataKey==).favoritingMetadataFavoritingKey) favs
          mFavKeys = map favoritingUserFavoritingKey mFavs
       in filter (\user -> (userKey user)`elem`mFavKeys) users

-- Builds a filter function for program lists from a program query.
-- @param progQuery - the program query containing the constraints
getFilter :: ProgramQuery -> IO (Filter Program)
getFilter (ProgramQuery mK kw (ti,d,ta) v l a fs _ _) =
  return $ filter (\prog -> 
    hasKey        prog &&
    hasKeyword    prog &&
    hasVisibility prog &&
    hasLanguage   prog &&
    hasAuthor     prog &&
    hasFavers     prog)
  where 
    hasKey prog = maybe True (programKey prog==) mK
    hasKeyword prog = 
      if null kw then True else or 
        [if ti
         then (lowercased kw)`isInfixOf`(lowercased$programTitle prog) 
         else False
        ,if d
         then (lowercased kw)`isInfixOf`(lowercased$programDescription prog)
         else False
        ,if ta
         then (lowercased kw)`elem`(map (lowercased . tagName)$programTags prog)
         else False]
    hasVisibility prog =
      if v
      then programIsVisible prog
      else True
    hasLanguage prog =
      if null l
      then True 
      else (lowercased l)==(lowercased$languageName$programImplLang prog)
    hasAuthor prog =
      if null a
      then True
      else a==(userName$programAuthor prog)
    hasFavers prog =
      if null fs
      then True
      else null (fs\\(map userName$programFavoriters prog))
    lowercased = map toLower

--------------------------------------------------------------------------------
-- Program sortings                                                           --
--------------------------------------------------------------------------------

--- Compares two programs regarding their titles.
leqProgramTitle :: Sorting Program
leqProgramTitle = (LEQ,\prog1 prog2 ->
  programTitle prog1 `leqStringIgnoreCase`
  programTitle prog2)

--- Compares two programs regarding the name of their implementation languages.
leqProgramImplLangName :: Sorting Program
leqProgramImplLangName = (LEQ,\prog1 prog2 ->
  (languageName $ programImplLang prog1) `leqStringIgnoreCase`
  (languageName $ programImplLang prog2))

--- Compares two programs regarding their creation dates.
leqProgramFirstVersionDate :: Sorting Program
leqProgramFirstVersionDate = (LEQ,\prog1 prog2 ->
  (versionDate $ programFirstVersion prog1) <=
  (versionDate $ programFirstVersion prog2))

--- Compares two programs regarding the dates of their last modification.
leqProgramLatestVersionDate :: Sorting Program
leqProgramLatestVersionDate = (LEQ,\prog1 prog2 ->
  (versionDate $ programLatestVersion prog1) <=
  (versionDate $ programLatestVersion prog2))

--- Compares two programs regarding the count of users who favorited them.
leqProgramFavoriterCount :: Sorting Program
leqProgramFavoriterCount = (LEQ,\prog1 prog2 ->
  (length $ programFavoriters prog1,versionDate $ programFirstVersion prog1) <=
  (length $ programFavoriters prog2,versionDate $ programFirstVersion prog2))

-------------------------------------------------------------------------------- 