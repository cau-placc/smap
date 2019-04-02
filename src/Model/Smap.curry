module Model.Smap (
 Metadata, Version, Language, System, User, Tag, Comment, MetadataKey,
 VersionKey, LanguageKey, SystemKey, UserKey, TagKey, CommentKey, Favoriting,
 Tagging, metadataTitle, setMetadataTitle, metadataDescription,
 setMetadataDescription, metadataIsVisible, setMetadataIsVisible,
 metadataLanguageImplLangKey, setMetadataLanguageImplLangKey,
 metadataUserAuthoringKey, setMetadataUserAuthoringKey, versionNumber,
 setVersionNumber, versionSourceCode, setVersionSourceCode, versionMessage,
 setVersionMessage, versionDate, setVersionDate, versionMetadataVersioningKey,
 setVersionMetadataVersioningKey, languageName, setLanguageName,
 languageFilenameExt, setLanguageFilenameExt, languageTemplate,
 setLanguageTemplate, systemName, setSystemName, systemExecUrl,
 setSystemExecUrl, systemLanguageLangImplKey, setSystemLanguageLangImplKey,
 userName, setUserName, userEmail, setUserEmail, userHash, setUserHash,
 userIsAdmin, setUserIsAdmin, tagName, setTagName, commentText,
 setCommentText, commentDate, setCommentDate, commentUserCAuthoringKey,
 setCommentUserCAuthoringKey, commentMetadataCommentingKey,
 setCommentMetadataCommentingKey, metadata, metadataKey, showMetadataKey,
 readMetadataKey, newMetadataWithLanguageImplLangKeyWithUserAuthoringKey,
 updateMetadata, deleteMetadata, getMetadata, queryAllMetadatas,
 queryCondMetadata, version, versionKey, showVersionKey, readVersionKey,
 newVersionWithMetadataVersioningKey, updateVersion, deleteVersion,
 getVersion, queryAllVersions, queryCondVersion, language, languageKey,
 showLanguageKey, readLanguageKey, newLanguage, updateLanguage,
 deleteLanguage, getLanguage, queryAllLanguages, queryCondLanguage, system,
 systemKey, showSystemKey, readSystemKey, newSystemWithLanguageLangImplKey,
 updateSystem, deleteSystem, getSystem, queryAllSystems, queryCondSystem,
 user, userKey, showUserKey, readUserKey, newUser, updateUser, deleteUser,
 getUser, queryAllUsers, queryCondUser, tag, tagKey, showTagKey, readTagKey,
 newTag, updateTag, deleteTag, getTag, queryAllTags, queryCondTag, comment,
 commentKey, showCommentKey, readCommentKey,
 newCommentWithUserCAuthoringKeyWithMetadataCommentingKey, updateComment,
 deleteComment, getComment, queryAllComments, queryCondComment,
 favoritingUserFavoritingKey, favoritingMetadataFavoritingKey, favoriting,
 newFavoriting, deleteFavoriting, getUserMetadatas, queryAllFavoritings,
 queryCondFavoriting, taggingMetadataTaggingKey, taggingTagTaggingKey,
 tagging, newTagging, deleteTagging, getMetadataTags, queryAllTaggings,
 queryCondTagging, langImpl, hasLangImpl, isALangImplOf, hasFavorite,
 isAFavoriteOf, commenting, hasComment, isACommentOf, cAuthoring,
 isTheCAuthorOf, hasCAuthor, hasTag, isATagOf, versioning, hasVersion,
 isAVersionOf, authoring, hasAuthor, isTheAuthorOf, implLang, hasImplLang,
 isTheImplLangOf, checkAllData, checkFavoriting, checkTagging, checkMetadata,
 checkVersion, checkLanguage, checkSystem, checkUser, checkTag, checkComment,
 saveAllData, restoreAllData
 ) where

import ERDGeneric
import KeyDatabase
import Time

import Config.Smap ( smapDB )

data Metadata
 = Metadata ERDGeneric.Key String String Bool ERDGeneric.Key ERDGeneric.Key 
 deriving Eq
type MetadataTuple = (String,String,Bool,ERDGeneric.Key,ERDGeneric.Key)
data Version
 = Version ERDGeneric.Key Int String String Time.CalendarTime ERDGeneric.Key 
 deriving Eq
type VersionTuple = (Int,String,String,Time.CalendarTime,ERDGeneric.Key)
data Language = Language ERDGeneric.Key String String String deriving Eq
type LanguageTuple = (String,String,String)
data System = System ERDGeneric.Key String String ERDGeneric.Key 
type SystemTuple = (String,String,ERDGeneric.Key)
data User = User ERDGeneric.Key String String String Bool deriving Eq
type UserTuple = (String,String,String,Bool)
data Tag = Tag ERDGeneric.Key String   deriving Eq
type TagTuple = String
data Comment
 = Comment ERDGeneric.Key String Time.CalendarTime ERDGeneric.Key
    ERDGeneric.Key deriving Eq
type CommentTuple = (String,Time.CalendarTime,ERDGeneric.Key,ERDGeneric.Key)
data MetadataKey = MetadataKey ERDGeneric.Key  deriving (Eq, Show)
data VersionKey = VersionKey ERDGeneric.Key 
data LanguageKey = LanguageKey ERDGeneric.Key deriving (Eq, Show)
data SystemKey = SystemKey ERDGeneric.Key 
data UserKey = UserKey ERDGeneric.Key  deriving (Eq, Show)
data TagKey = TagKey ERDGeneric.Key  deriving (Eq, Show)
data CommentKey = CommentKey ERDGeneric.Key 
data Favoriting = Favoriting ERDGeneric.Key ERDGeneric.Key 
type FavoritingTuple = (ERDGeneric.Key,ERDGeneric.Key)
data Tagging = Tagging ERDGeneric.Key ERDGeneric.Key 
type TaggingTuple = (ERDGeneric.Key,ERDGeneric.Key)

--- Transforms entity Metadata into tuple representation.
metadata2tuple :: Metadata -> MetadataTuple
metadata2tuple (Metadata _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a Metadata entity.
keytuple2Metadata :: ERDGeneric.Key -> MetadataTuple -> Metadata
keytuple2Metadata x1 (x2 ,x3 ,x4 ,x5 ,x6) = Metadata x1 x2 x3 x4 x5 x6

--- Transforms entity Version into tuple representation.
version2tuple :: Version -> VersionTuple
version2tuple (Version _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a Version entity.
keytuple2Version :: ERDGeneric.Key -> VersionTuple -> Version
keytuple2Version x1 (x2 ,x3 ,x4 ,x5 ,x6) = Version x1 x2 x3 x4 x5 x6

--- Transforms entity Language into tuple representation.
language2tuple :: Language -> LanguageTuple
language2tuple (Language _ x2 x3 x4) = (x2,x3,x4)

--- Transforms key and tuple into a Language entity.
keytuple2Language :: ERDGeneric.Key -> LanguageTuple -> Language
keytuple2Language x1 (x2 ,x3 ,x4) = Language x1 x2 x3 x4

--- Transforms entity System into tuple representation.
system2tuple :: System -> SystemTuple
system2tuple (System _ x2 x3 x4) = (x2,x3,x4)

--- Transforms key and tuple into a System entity.
keytuple2System :: ERDGeneric.Key -> SystemTuple -> System
keytuple2System x1 (x2 ,x3 ,x4) = System x1 x2 x3 x4

--- Transforms entity User into tuple representation.
user2tuple :: User -> UserTuple
user2tuple (User _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a User entity.
keytuple2User :: ERDGeneric.Key -> UserTuple -> User
keytuple2User x1 (x2 ,x3 ,x4 ,x5) = User x1 x2 x3 x4 x5

--- Transforms entity Tag into tuple representation.
tag2tuple :: Tag -> TagTuple
tag2tuple (Tag _ x2) = x2

--- Transforms key and tuple into a Tag entity.
keytuple2Tag :: ERDGeneric.Key -> TagTuple -> Tag
keytuple2Tag x1 x2 = Tag x1 x2

--- Transforms entity Comment into tuple representation.
comment2tuple :: Comment -> CommentTuple
comment2tuple (Comment _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a Comment entity.
keytuple2Comment :: ERDGeneric.Key -> CommentTuple -> Comment
keytuple2Comment x1 (x2 ,x3 ,x4 ,x5) = Comment x1 x2 x3 x4 x5

--- Transforms relationship entity Favoriting into tuple representation.
favoriting2tuple :: Favoriting -> FavoritingTuple
favoriting2tuple (Favoriting x1 x2) = (x1,x2)

--- Transforms key and tuple into a Favoriting relationship entity.
keytuple2Favoriting :: ERDGeneric.Key -> FavoritingTuple -> Favoriting
keytuple2Favoriting _ (x1 ,x2) = Favoriting x1 x2

--- Transforms relationship entity Tagging into tuple representation.
tagging2tuple :: Tagging -> TaggingTuple
tagging2tuple (Tagging x1 x2) = (x1,x2)

--- Transforms key and tuple into a Tagging relationship entity.
keytuple2Tagging :: ERDGeneric.Key -> TaggingTuple -> Tagging
keytuple2Tagging _ (x1 ,x2) = Tagging x1 x2

--- Sets the value of attribute "UserFavoritingKey" in a Favoriting entity.
setFavoritingUserFavoritingKey :: Favoriting -> UserKey -> Favoriting
setFavoritingUserFavoritingKey (Favoriting _ x2) x =
  Favoriting (userKeyToKey x) x2

--- Sets the value of attribute "MetadataFavoritingKey" in a Favoriting entity.
setFavoritingMetadataFavoritingKey :: Favoriting -> MetadataKey -> Favoriting
setFavoritingMetadataFavoritingKey (Favoriting x1 _) x =
  Favoriting x1 (metadataKeyToKey x)

--- Sets the value of attribute "MetadataTaggingKey" in a Tagging entity.
setTaggingMetadataTaggingKey :: Tagging -> MetadataKey -> Tagging
setTaggingMetadataTaggingKey (Tagging _ x2) x =
  Tagging (metadataKeyToKey x) x2

--- Sets the value of attribute "TagTaggingKey" in a Tagging entity.
setTaggingTagTaggingKey :: Tagging -> TagKey -> Tagging
setTaggingTagTaggingKey (Tagging x1 _) x = Tagging x1 (tagKeyToKey x)

--- Sets the value of attribute "Key" in a Metadata entity.
setMetadataKey :: Metadata -> ERDGeneric.Key -> Metadata
setMetadataKey (Metadata _ x2 x3 x4 x5 x6) x = Metadata x x2 x3 x4 x5 x6

--- Gets the value of attribute "Title" of a Metadata entity.
metadataTitle :: Metadata -> String
metadataTitle (Metadata _ x _ _ _ _) = x

--- Sets the value of attribute "Title" in a Metadata entity.
setMetadataTitle :: Metadata -> String -> Metadata
setMetadataTitle (Metadata x1 _ x3 x4 x5 x6) x = Metadata x1 x x3 x4 x5 x6

--- Gets the value of attribute "Description" of a Metadata entity.
metadataDescription :: Metadata -> String
metadataDescription (Metadata _ _ x _ _ _) = x

--- Sets the value of attribute "Description" in a Metadata entity.
setMetadataDescription :: Metadata -> String -> Metadata
setMetadataDescription (Metadata x1 x2 _ x4 x5 x6) x =
  Metadata x1 x2 x x4 x5 x6

--- Gets the value of attribute "IsVisible" of a Metadata entity.
metadataIsVisible :: Metadata -> Bool
metadataIsVisible (Metadata _ _ _ x _ _) = x

--- Sets the value of attribute "IsVisible" in a Metadata entity.
setMetadataIsVisible :: Metadata -> Bool -> Metadata
setMetadataIsVisible (Metadata x1 x2 x3 _ x5 x6) x = Metadata x1 x2 x3 x x5 x6

--- Gets the value of attribute "LanguageImplLangKey" of a Metadata entity.
metadataLanguageImplLangKey :: Metadata -> LanguageKey
metadataLanguageImplLangKey (Metadata _ _ _ _ x _) = LanguageKey x

--- Sets the value of attribute "LanguageImplLangKey" in a Metadata entity.
setMetadataLanguageImplLangKey :: Metadata -> LanguageKey -> Metadata
setMetadataLanguageImplLangKey (Metadata x1 x2 x3 x4 _ x6) x =
  Metadata x1 x2 x3 x4 (languageKeyToKey x) x6

--- Gets the value of attribute "UserAuthoringKey" of a Metadata entity.
metadataUserAuthoringKey :: Metadata -> UserKey
metadataUserAuthoringKey (Metadata _ _ _ _ _ x) = UserKey x

--- Sets the value of attribute "UserAuthoringKey" in a Metadata entity.
setMetadataUserAuthoringKey :: Metadata -> UserKey -> Metadata
setMetadataUserAuthoringKey (Metadata x1 x2 x3 x4 x5 _) x =
  Metadata x1 x2 x3 x4 x5 (userKeyToKey x)

--- Sets the value of attribute "Key" in a Version entity.
setVersionKey :: Version -> ERDGeneric.Key -> Version
setVersionKey (Version _ x2 x3 x4 x5 x6) x = Version x x2 x3 x4 x5 x6

--- Gets the value of attribute "Number" of a Version entity.
versionNumber :: Version -> Int
versionNumber (Version _ x _ _ _ _) = x

--- Sets the value of attribute "Number" in a Version entity.
setVersionNumber :: Version -> Int -> Version
setVersionNumber (Version x1 _ x3 x4 x5 x6) x = Version x1 x x3 x4 x5 x6

--- Gets the value of attribute "SourceCode" of a Version entity.
versionSourceCode :: Version -> String
versionSourceCode (Version _ _ x _ _ _) = x

--- Sets the value of attribute "SourceCode" in a Version entity.
setVersionSourceCode :: Version -> String -> Version
setVersionSourceCode (Version x1 x2 _ x4 x5 x6) x = Version x1 x2 x x4 x5 x6

--- Gets the value of attribute "Message" of a Version entity.
versionMessage :: Version -> String
versionMessage (Version _ _ _ x _ _) = x

--- Sets the value of attribute "Message" in a Version entity.
setVersionMessage :: Version -> String -> Version
setVersionMessage (Version x1 x2 x3 _ x5 x6) x = Version x1 x2 x3 x x5 x6

--- Gets the value of attribute "Date" of a Version entity.
versionDate :: Version -> Time.CalendarTime
versionDate (Version _ _ _ _ x _) = x

--- Sets the value of attribute "Date" in a Version entity.
setVersionDate :: Version -> Time.CalendarTime -> Version
setVersionDate (Version x1 x2 x3 x4 _ x6) x = Version x1 x2 x3 x4 x x6

--- Gets the value of attribute "MetadataVersioningKey" of a Version entity.
versionMetadataVersioningKey :: Version -> MetadataKey
versionMetadataVersioningKey (Version _ _ _ _ _ x) = MetadataKey x

--- Sets the value of attribute "MetadataVersioningKey" in a Version entity.
setVersionMetadataVersioningKey :: Version -> MetadataKey -> Version
setVersionMetadataVersioningKey (Version x1 x2 x3 x4 x5 _) x =
  Version x1 x2 x3 x4 x5 (metadataKeyToKey x)

--- Sets the value of attribute "Key" in a Language entity.
setLanguageKey :: Language -> ERDGeneric.Key -> Language
setLanguageKey (Language _ x2 x3 x4) x = Language x x2 x3 x4

--- Gets the value of attribute "Name" of a Language entity.
languageName :: Language -> String
languageName (Language _ x _ _) = x

--- Sets the value of attribute "Name" in a Language entity.
setLanguageName :: Language -> String -> Language
setLanguageName (Language x1 _ x3 x4) x = Language x1 x x3 x4

--- Gets the value of attribute "FilenameExt" of a Language entity.
languageFilenameExt :: Language -> String
languageFilenameExt (Language _ _ x _) = x

--- Sets the value of attribute "FilenameExt" in a Language entity.
setLanguageFilenameExt :: Language -> String -> Language
setLanguageFilenameExt (Language x1 x2 _ x4) x = Language x1 x2 x x4

--- Gets the value of attribute "Template" of a Language entity.
languageTemplate :: Language -> String
languageTemplate (Language _ _ _ x) = x

--- Sets the value of attribute "Template" in a Language entity.
setLanguageTemplate :: Language -> String -> Language
setLanguageTemplate (Language x1 x2 x3 _) x = Language x1 x2 x3 x

--- Sets the value of attribute "Key" in a System entity.
setSystemKey :: System -> ERDGeneric.Key -> System
setSystemKey (System _ x2 x3 x4) x = System x x2 x3 x4

--- Gets the value of attribute "Name" of a System entity.
systemName :: System -> String
systemName (System _ x _ _) = x

--- Sets the value of attribute "Name" in a System entity.
setSystemName :: System -> String -> System
setSystemName (System x1 _ x3 x4) x = System x1 x x3 x4

--- Gets the value of attribute "ExecUrl" of a System entity.
systemExecUrl :: System -> String
systemExecUrl (System _ _ x _) = x

--- Sets the value of attribute "ExecUrl" in a System entity.
setSystemExecUrl :: System -> String -> System
setSystemExecUrl (System x1 x2 _ x4) x = System x1 x2 x x4

--- Gets the value of attribute "LanguageLangImplKey" of a System entity.
systemLanguageLangImplKey :: System -> LanguageKey
systemLanguageLangImplKey (System _ _ _ x) = LanguageKey x

--- Sets the value of attribute "LanguageLangImplKey" in a System entity.
setSystemLanguageLangImplKey :: System -> LanguageKey -> System
setSystemLanguageLangImplKey (System x1 x2 x3 _) x =
  System x1 x2 x3 (languageKeyToKey x)

--- Sets the value of attribute "Key" in a User entity.
setUserKey :: User -> ERDGeneric.Key -> User
setUserKey (User _ x2 x3 x4 x5) x = User x x2 x3 x4 x5

--- Gets the value of attribute "Name" of a User entity.
userName :: User -> String
userName (User _ x _ _ _) = x

--- Sets the value of attribute "Name" in a User entity.
setUserName :: User -> String -> User
setUserName (User x1 _ x3 x4 x5) x = User x1 x x3 x4 x5

--- Gets the value of attribute "Email" of a User entity.
userEmail :: User -> String
userEmail (User _ _ x _ _) = x

--- Sets the value of attribute "Email" in a User entity.
setUserEmail :: User -> String -> User
setUserEmail (User x1 x2 _ x4 x5) x = User x1 x2 x x4 x5

--- Gets the value of attribute "Hash" of a User entity.
userHash :: User -> String
userHash (User _ _ _ x _) = x

--- Sets the value of attribute "Hash" in a User entity.
setUserHash :: User -> String -> User
setUserHash (User x1 x2 x3 _ x5) x = User x1 x2 x3 x x5

--- Gets the value of attribute "IsAdmin" of a User entity.
userIsAdmin :: User -> Bool
userIsAdmin (User _ _ _ _ x) = x

--- Sets the value of attribute "IsAdmin" in a User entity.
setUserIsAdmin :: User -> Bool -> User
setUserIsAdmin (User x1 x2 x3 x4 _) x = User x1 x2 x3 x4 x

--- Sets the value of attribute "Key" in a Tag entity.
setTagKey :: Tag -> ERDGeneric.Key -> Tag
setTagKey (Tag _ x2) x = Tag x x2

--- Gets the value of attribute "Name" of a Tag entity.
tagName :: Tag -> String
tagName (Tag _ x) = x

--- Sets the value of attribute "Name" in a Tag entity.
setTagName :: Tag -> String -> Tag
setTagName (Tag x1 _) x = Tag x1 x

--- Sets the value of attribute "Key" in a Comment entity.
setCommentKey :: Comment -> ERDGeneric.Key -> Comment
setCommentKey (Comment _ x2 x3 x4 x5) x = Comment x x2 x3 x4 x5

--- Gets the value of attribute "Text" of a Comment entity.
commentText :: Comment -> String
commentText (Comment _ x _ _ _) = x

--- Sets the value of attribute "Text" in a Comment entity.
setCommentText :: Comment -> String -> Comment
setCommentText (Comment x1 _ x3 x4 x5) x = Comment x1 x x3 x4 x5

--- Gets the value of attribute "Date" of a Comment entity.
commentDate :: Comment -> Time.CalendarTime
commentDate (Comment _ _ x _ _) = x

--- Sets the value of attribute "Date" in a Comment entity.
setCommentDate :: Comment -> Time.CalendarTime -> Comment
setCommentDate (Comment x1 x2 _ x4 x5) x = Comment x1 x2 x x4 x5

--- Gets the value of attribute "UserCAuthoringKey" of a Comment entity.
commentUserCAuthoringKey :: Comment -> UserKey
commentUserCAuthoringKey (Comment _ _ _ x _) = UserKey x

--- Sets the value of attribute "UserCAuthoringKey" in a Comment entity.
setCommentUserCAuthoringKey :: Comment -> UserKey -> Comment
setCommentUserCAuthoringKey (Comment x1 x2 x3 _ x5) x =
  Comment x1 x2 x3 (userKeyToKey x) x5

--- Gets the value of attribute "MetadataCommentingKey" of a Comment entity.
commentMetadataCommentingKey :: Comment -> MetadataKey
commentMetadataCommentingKey (Comment _ _ _ _ x) = MetadataKey x

--- Sets the value of attribute "MetadataCommentingKey" in a Comment entity.
setCommentMetadataCommentingKey :: Comment -> MetadataKey -> Comment
setCommentMetadataCommentingKey (Comment x1 x2 x3 x4 _) x =
  Comment x1 x2 x3 x4 (metadataKeyToKey x)

--- Database predicate representing the relation between keys and Metadata tuple entities.
metadataEntry :: ERDGeneric.Key -> MetadataTuple -> KeyDatabase.Dynamic
metadataEntry =
  KeyDatabase.persistentSQLite smapDB "Metadata"
   ["Title","Description","IsVisible","LanguageImplLangKey"
   ,"UserAuthoringKey"]

--- Dynamic predicate representing the relation
--- between keys and Metadata entities.
metadata :: MetadataKey -> Metadata -> KeyDatabase.Dynamic
metadata key obj
  | key =:= metadataKey obj = metadataEntry (metadataKeyToKey key)
                               (metadata2tuple obj)

--- Gets the key of a Metadata entity.
metadataKey :: Metadata -> MetadataKey
metadataKey (Metadata x _ _ _ _ _) = MetadataKey x

--- Shows the key of a Metadata entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMetadataKey :: Metadata -> String
showMetadataKey obj =
  ERDGeneric.showDatabaseKey "Metadata" metadataKeyToKey (metadataKey obj)

--- Transforms a string into a key of a Metadata entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMetadataKey :: String -> Maybe MetadataKey
readMetadataKey s = ERDGeneric.readDatabaseKey "Metadata" MetadataKey s

metadataKeyToKey :: MetadataKey -> ERDGeneric.Key
metadataKeyToKey (MetadataKey k) = k

maybeMetadataKeyToKey :: Maybe MetadataKey -> Maybe ERDGeneric.Key
maybeMetadataKeyToKey Nothing = Nothing
maybeMetadataKeyToKey (Just (MetadataKey k)) = Just k

--- Inserts a new Metadata entity.
newMetadataWithLanguageImplLangKeyWithUserAuthoringKey
 :: String -> String -> Bool -> LanguageKey -> UserKey
  -> KeyDatabase.Transaction Metadata
newMetadataWithLanguageImplLangKeyWithUserAuthoringKey title_p description_p
                                                       isVisible_p
                                                       languageImplLangKey_p
                                                       userAuthoringKey_p =
  ERDGeneric.existsEntryWithDBKey "Language" languageEntry
   (languageKeyToKey languageImplLangKey_p) |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey userAuthoringKey_p) |>>
    ERDGeneric.newEntry metadataEntry keytuple2Metadata
     (title_p,description_p,isVisible_p,languageKeyToKey languageImplLangKey_p
     ,userKeyToKey userAuthoringKey_p))

--- Updates an existing Metadata entity.
updateMetadata :: Metadata -> KeyDatabase.Transaction ()
updateMetadata metadata_p =
  ERDGeneric.existsEntryWithDBKey "Language" languageEntry
   (languageKeyToKey (metadataLanguageImplLangKey metadata_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey (metadataUserAuthoringKey metadata_p)) |>>
    KeyDatabase.updateDBEntry metadataEntry
     (metadataKeyToKey (metadataKey metadata_p)) (metadata2tuple metadata_p))

--- Deletes an existing Metadata entity.
deleteMetadata :: Metadata -> KeyDatabase.Transaction ()
deleteMetadata metadata_p =
  ERDGeneric.requiredForeignDBKey "Favoriting" favoritingEntry
   keytuple2Favoriting favoritingMetadataFavoritingKey
   (metadataKey metadata_p) |>>
   (ERDGeneric.requiredForeignDBKey "Tagging" taggingEntry keytuple2Tagging
     taggingMetadataTaggingKey (metadataKey metadata_p) |>>
    (ERDGeneric.requiredForeignDBKey "Version" versionEntry keytuple2Version
      versionMetadataVersioningKey (metadataKey metadata_p) |>>
     (ERDGeneric.requiredForeignDBKey "Comment" commentEntry keytuple2Comment
       commentMetadataCommentingKey (metadataKey metadata_p) |>>
      KeyDatabase.deleteDBEntry metadataEntry
       (metadataKeyToKey (metadataKey metadata_p)))))

--- Gets a Metadata entity stored in the database with the given key.
getMetadata :: MetadataKey -> KeyDatabase.Transaction Metadata
getMetadata key =
  ERDGeneric.getEntry metadataEntry keytuple2Metadata (metadataKeyToKey key)

--- Gets all Metadata entities stored in the database.
queryAllMetadatas :: KeyDatabase.Query [Metadata]
queryAllMetadatas =
  KeyDatabase.transformQ (map (uncurry keytuple2Metadata))
   (KeyDatabase.allDBKeyInfos metadataEntry)

--- Gets all Metadata entities satisfying a given condition.
queryCondMetadata :: (Metadata -> Bool) -> KeyDatabase.Query [Metadata]
queryCondMetadata econd =
  KeyDatabase.transformQ (filter econd) queryAllMetadatas

--- Database predicate representing the relation between keys and Version tuple entities.
versionEntry :: ERDGeneric.Key -> VersionTuple -> KeyDatabase.Dynamic
versionEntry =
  KeyDatabase.persistentSQLite smapDB "Version"
   ["Number","SourceCode","Message","Date","MetadataVersioningKey"]

--- Dynamic predicate representing the relation
--- between keys and Version entities.
version :: VersionKey -> Version -> KeyDatabase.Dynamic
version key obj
  | key =:= versionKey obj = versionEntry (versionKeyToKey key)
                              (version2tuple obj)

--- Gets the key of a Version entity.
versionKey :: Version -> VersionKey
versionKey (Version x _ _ _ _ _) = VersionKey x

--- Shows the key of a Version entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showVersionKey :: Version -> String
showVersionKey obj =
  ERDGeneric.showDatabaseKey "Version" versionKeyToKey (versionKey obj)

--- Transforms a string into a key of a Version entity.
--- Nothing is returned if the string does not represent a reasonable key.
readVersionKey :: String -> Maybe VersionKey
readVersionKey s = ERDGeneric.readDatabaseKey "Version" VersionKey s

versionKeyToKey :: VersionKey -> ERDGeneric.Key
versionKeyToKey (VersionKey k) = k

maybeVersionKeyToKey :: Maybe VersionKey -> Maybe ERDGeneric.Key
maybeVersionKeyToKey Nothing = Nothing
maybeVersionKeyToKey (Just (VersionKey k)) = Just k

--- Inserts a new Version entity.
newVersionWithMetadataVersioningKey
 :: Int -> String -> String -> Time.CalendarTime -> MetadataKey
  -> KeyDatabase.Transaction Version
newVersionWithMetadataVersioningKey number_p sourceCode_p message_p date_p
                                    metadataVersioningKey_p =
  ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
   (metadataKeyToKey metadataVersioningKey_p) |>>
   ERDGeneric.newEntry versionEntry keytuple2Version
    (number_p,sourceCode_p,message_p,date_p
    ,metadataKeyToKey metadataVersioningKey_p)

--- Updates an existing Version entity.
updateVersion :: Version -> KeyDatabase.Transaction ()
updateVersion version_p =
  ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
   (metadataKeyToKey (versionMetadataVersioningKey version_p)) |>>
   KeyDatabase.updateDBEntry versionEntry
    (versionKeyToKey (versionKey version_p)) (version2tuple version_p)

--- Deletes an existing Version entity.
deleteVersion :: Version -> KeyDatabase.Transaction ()
deleteVersion version_p =
  KeyDatabase.deleteDBEntry versionEntry
   (versionKeyToKey (versionKey version_p))

--- Gets a Version entity stored in the database with the given key.
getVersion :: VersionKey -> KeyDatabase.Transaction Version
getVersion key =
  ERDGeneric.getEntry versionEntry keytuple2Version (versionKeyToKey key)

--- Gets all Version entities stored in the database.
queryAllVersions :: KeyDatabase.Query [Version]
queryAllVersions =
  KeyDatabase.transformQ (map (uncurry keytuple2Version))
   (KeyDatabase.allDBKeyInfos versionEntry)

--- Gets all Version entities satisfying a given condition.
queryCondVersion :: (Version -> Bool) -> KeyDatabase.Query [Version]
queryCondVersion econd =
  KeyDatabase.transformQ (filter econd) queryAllVersions

--- Database predicate representing the relation between keys and Language tuple entities.
languageEntry :: ERDGeneric.Key -> LanguageTuple -> KeyDatabase.Dynamic
languageEntry =
  KeyDatabase.persistentSQLite smapDB "Language"
   ["Name","FilenameExt","Template"]

--- Dynamic predicate representing the relation
--- between keys and Language entities.
language :: LanguageKey -> Language -> KeyDatabase.Dynamic
language key obj
  | key =:= languageKey obj = languageEntry (languageKeyToKey key)
                               (language2tuple obj)

--- Gets the key of a Language entity.
languageKey :: Language -> LanguageKey
languageKey (Language x _ _ _) = LanguageKey x

--- Shows the key of a Language entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showLanguageKey :: Language -> String
showLanguageKey obj =
  ERDGeneric.showDatabaseKey "Language" languageKeyToKey (languageKey obj)

--- Transforms a string into a key of a Language entity.
--- Nothing is returned if the string does not represent a reasonable key.
readLanguageKey :: String -> Maybe LanguageKey
readLanguageKey s = ERDGeneric.readDatabaseKey "Language" LanguageKey s

languageKeyToKey :: LanguageKey -> ERDGeneric.Key
languageKeyToKey (LanguageKey k) = k

maybeLanguageKeyToKey :: Maybe LanguageKey -> Maybe ERDGeneric.Key
maybeLanguageKeyToKey Nothing = Nothing
maybeLanguageKeyToKey (Just (LanguageKey k)) = Just k

--- Inserts a new Language entity.
newLanguage :: String -> String -> String -> KeyDatabase.Transaction Language
newLanguage name_p filenameExt_p template_p =
  ERDGeneric.unique "Smap" languageEntry keytuple2Language languageName name_p
   |>>
   (ERDGeneric.unique "Smap" languageEntry keytuple2Language
     languageFilenameExt filenameExt_p |>>
    ERDGeneric.newEntry languageEntry keytuple2Language
     (name_p,filenameExt_p,template_p))

--- Updates an existing Language entity.
updateLanguage :: Language -> KeyDatabase.Transaction ()
updateLanguage language_p =
  ERDGeneric.uniqueUpdate "Smap" languageEntry keytuple2Language
   (languageKeyToKey . languageKey) languageName language_p |>>
   (ERDGeneric.uniqueUpdate "Smap" languageEntry keytuple2Language
     (languageKeyToKey . languageKey) languageFilenameExt language_p |>>
    KeyDatabase.updateDBEntry languageEntry
     (languageKeyToKey (languageKey language_p)) (language2tuple language_p))

--- Deletes an existing Language entity.
deleteLanguage :: Language -> KeyDatabase.Transaction ()
deleteLanguage language_p =
  ERDGeneric.requiredForeignDBKey "Metadata" metadataEntry keytuple2Metadata
   metadataLanguageImplLangKey (languageKey language_p) |>>
   (ERDGeneric.requiredForeignDBKey "System" systemEntry keytuple2System
     systemLanguageLangImplKey (languageKey language_p) |>>
    KeyDatabase.deleteDBEntry languageEntry
     (languageKeyToKey (languageKey language_p)))

--- Gets a Language entity stored in the database with the given key.
getLanguage :: LanguageKey -> KeyDatabase.Transaction Language
getLanguage key =
  ERDGeneric.getEntry languageEntry keytuple2Language (languageKeyToKey key)

--- Gets all Language entities stored in the database.
queryAllLanguages :: KeyDatabase.Query [Language]
queryAllLanguages =
  KeyDatabase.transformQ (map (uncurry keytuple2Language))
   (KeyDatabase.allDBKeyInfos languageEntry)

--- Gets all Language entities satisfying a given condition.
queryCondLanguage :: (Language -> Bool) -> KeyDatabase.Query [Language]
queryCondLanguage econd =
  KeyDatabase.transformQ (filter econd) queryAllLanguages

--- Database predicate representing the relation between keys and System tuple entities.
systemEntry :: ERDGeneric.Key -> SystemTuple -> KeyDatabase.Dynamic
systemEntry =
  KeyDatabase.persistentSQLite smapDB "System"
   ["Name","ExecUrl","LanguageLangImplKey"]

--- Dynamic predicate representing the relation
--- between keys and System entities.
system :: SystemKey -> System -> KeyDatabase.Dynamic
system key obj
  | key =:= systemKey obj = systemEntry (systemKeyToKey key)
                             (system2tuple obj)

--- Gets the key of a System entity.
systemKey :: System -> SystemKey
systemKey (System x _ _ _) = SystemKey x

--- Shows the key of a System entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showSystemKey :: System -> String
showSystemKey obj =
  ERDGeneric.showDatabaseKey "System" systemKeyToKey (systemKey obj)

--- Transforms a string into a key of a System entity.
--- Nothing is returned if the string does not represent a reasonable key.
readSystemKey :: String -> Maybe SystemKey
readSystemKey s = ERDGeneric.readDatabaseKey "System" SystemKey s

systemKeyToKey :: SystemKey -> ERDGeneric.Key
systemKeyToKey (SystemKey k) = k

maybeSystemKeyToKey :: Maybe SystemKey -> Maybe ERDGeneric.Key
maybeSystemKeyToKey Nothing = Nothing
maybeSystemKeyToKey (Just (SystemKey k)) = Just k

--- Inserts a new System entity.
newSystemWithLanguageLangImplKey
 :: String -> String -> LanguageKey -> KeyDatabase.Transaction System
newSystemWithLanguageLangImplKey name_p execUrl_p languageLangImplKey_p =
  ERDGeneric.unique "Smap" systemEntry keytuple2System systemName name_p |>>
   (ERDGeneric.unique "Smap" systemEntry keytuple2System systemExecUrl
     execUrl_p |>>
    (ERDGeneric.existsEntryWithDBKey "Language" languageEntry
      (languageKeyToKey languageLangImplKey_p) |>>
     ERDGeneric.newEntry systemEntry keytuple2System
      (name_p,execUrl_p,languageKeyToKey languageLangImplKey_p)))

--- Updates an existing System entity.
updateSystem :: System -> KeyDatabase.Transaction ()
updateSystem system_p =
  ERDGeneric.uniqueUpdate "Smap" systemEntry keytuple2System
   (systemKeyToKey . systemKey) systemName system_p |>>
   (ERDGeneric.uniqueUpdate "Smap" systemEntry keytuple2System
     (systemKeyToKey . systemKey) systemExecUrl system_p |>>
    (ERDGeneric.existsEntryWithDBKey "Language" languageEntry
      (languageKeyToKey (systemLanguageLangImplKey system_p)) |>>
     KeyDatabase.updateDBEntry systemEntry
      (systemKeyToKey (systemKey system_p)) (system2tuple system_p)))

--- Deletes an existing System entity.
deleteSystem :: System -> KeyDatabase.Transaction ()
deleteSystem system_p =
  KeyDatabase.deleteDBEntry systemEntry (systemKeyToKey (systemKey system_p))

--- Gets a System entity stored in the database with the given key.
getSystem :: SystemKey -> KeyDatabase.Transaction System
getSystem key =
  ERDGeneric.getEntry systemEntry keytuple2System (systemKeyToKey key)

--- Gets all System entities stored in the database.
queryAllSystems :: KeyDatabase.Query [System]
queryAllSystems =
  KeyDatabase.transformQ (map (uncurry keytuple2System))
   (KeyDatabase.allDBKeyInfos systemEntry)

--- Gets all System entities satisfying a given condition.
queryCondSystem :: (System -> Bool) -> KeyDatabase.Query [System]
queryCondSystem econd = KeyDatabase.transformQ (filter econd) queryAllSystems

--- Database predicate representing the relation between keys and User tuple entities.
userEntry :: ERDGeneric.Key -> UserTuple -> KeyDatabase.Dynamic
userEntry =
  KeyDatabase.persistentSQLite smapDB "User"
   ["Name","Email","Hash","IsAdmin"]

--- Dynamic predicate representing the relation
--- between keys and User entities.
user :: UserKey -> User -> KeyDatabase.Dynamic
user key obj
  | key =:= userKey obj = userEntry (userKeyToKey key) (user2tuple obj)

--- Gets the key of a User entity.
userKey :: User -> UserKey
userKey (User x _ _ _ _) = UserKey x

--- Shows the key of a User entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUserKey :: User -> String
showUserKey obj = ERDGeneric.showDatabaseKey "User" userKeyToKey (userKey obj)

--- Transforms a string into a key of a User entity.
--- Nothing is returned if the string does not represent a reasonable key.
readUserKey :: String -> Maybe UserKey
readUserKey s = ERDGeneric.readDatabaseKey "User" UserKey s

userKeyToKey :: UserKey -> ERDGeneric.Key
userKeyToKey (UserKey k) = k

maybeUserKeyToKey :: Maybe UserKey -> Maybe ERDGeneric.Key
maybeUserKeyToKey Nothing = Nothing
maybeUserKeyToKey (Just (UserKey k)) = Just k

--- Inserts a new User entity.
newUser
 :: String -> String -> String -> Maybe Bool -> KeyDatabase.Transaction User
newUser name_p email_p hash_p isAdmin_p =
  ERDGeneric.unique "Smap" userEntry keytuple2User userName name_p |>>
   (ERDGeneric.unique "Smap" userEntry keytuple2User userEmail email_p |>>
    ERDGeneric.newEntry userEntry keytuple2User
     (name_p,email_p,hash_p,maybe False id isAdmin_p))

--- Updates an existing User entity.
updateUser :: User -> KeyDatabase.Transaction ()
updateUser user_p =
  ERDGeneric.uniqueUpdate "Smap" userEntry keytuple2User
   (userKeyToKey . userKey) userName user_p |>>
   (ERDGeneric.uniqueUpdate "Smap" userEntry keytuple2User
     (userKeyToKey . userKey) userEmail user_p |>>
    KeyDatabase.updateDBEntry userEntry (userKeyToKey (userKey user_p))
     (user2tuple user_p))

--- Deletes an existing User entity.
deleteUser :: User -> KeyDatabase.Transaction ()
deleteUser user_p =
  ERDGeneric.requiredForeignDBKey "Favoriting" favoritingEntry
   keytuple2Favoriting favoritingUserFavoritingKey (userKey user_p) |>>
   (ERDGeneric.requiredForeignDBKey "Metadata" metadataEntry keytuple2Metadata
     metadataUserAuthoringKey (userKey user_p) |>>
    (ERDGeneric.requiredForeignDBKey "Comment" commentEntry keytuple2Comment
      commentUserCAuthoringKey (userKey user_p) |>>
     KeyDatabase.deleteDBEntry userEntry (userKeyToKey (userKey user_p))))

--- Gets a User entity stored in the database with the given key.
getUser :: UserKey -> KeyDatabase.Transaction User
getUser key = ERDGeneric.getEntry userEntry keytuple2User (userKeyToKey key)

--- Gets all User entities stored in the database.
queryAllUsers :: KeyDatabase.Query [User]
queryAllUsers =
  KeyDatabase.transformQ (map (uncurry keytuple2User))
   (KeyDatabase.allDBKeyInfos userEntry)

--- Gets all User entities satisfying a given condition.
queryCondUser :: (User -> Bool) -> KeyDatabase.Query [User]
queryCondUser econd = KeyDatabase.transformQ (filter econd) queryAllUsers

--- Database predicate representing the relation between keys and Tag tuple entities.
tagEntry :: ERDGeneric.Key -> TagTuple -> KeyDatabase.Dynamic
tagEntry = KeyDatabase.persistentSQLite smapDB "Tag" ["Name"]

--- Dynamic predicate representing the relation
--- between keys and Tag entities.
tag :: TagKey -> Tag -> KeyDatabase.Dynamic
tag key obj
  | key =:= tagKey obj = tagEntry (tagKeyToKey key) (tag2tuple obj)

--- Gets the key of a Tag entity.
tagKey :: Tag -> TagKey
tagKey (Tag x _) = TagKey x

--- Shows the key of a Tag entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showTagKey :: Tag -> String
showTagKey obj = ERDGeneric.showDatabaseKey "Tag" tagKeyToKey (tagKey obj)

--- Transforms a string into a key of a Tag entity.
--- Nothing is returned if the string does not represent a reasonable key.
readTagKey :: String -> Maybe TagKey
readTagKey s = ERDGeneric.readDatabaseKey "Tag" TagKey s

tagKeyToKey :: TagKey -> ERDGeneric.Key
tagKeyToKey (TagKey k) = k

maybeTagKeyToKey :: Maybe TagKey -> Maybe ERDGeneric.Key
maybeTagKeyToKey Nothing = Nothing
maybeTagKeyToKey (Just (TagKey k)) = Just k

--- Inserts a new Tag entity.
newTag :: String -> KeyDatabase.Transaction Tag
newTag name_p =
  ERDGeneric.unique "Smap" tagEntry keytuple2Tag tagName name_p |>>
   ERDGeneric.newEntry tagEntry keytuple2Tag name_p

--- Updates an existing Tag entity.
updateTag :: Tag -> KeyDatabase.Transaction ()
updateTag tag_p =
  ERDGeneric.uniqueUpdate "Smap" tagEntry keytuple2Tag (tagKeyToKey . tagKey)
   tagName tag_p |>>
   KeyDatabase.updateDBEntry tagEntry (tagKeyToKey (tagKey tag_p))
    (tag2tuple tag_p)

--- Deletes an existing Tag entity.
deleteTag :: Tag -> KeyDatabase.Transaction ()
deleteTag tag_p =
  ERDGeneric.requiredForeignDBKey "Tagging" taggingEntry keytuple2Tagging
   taggingTagTaggingKey (tagKey tag_p) |>>
   KeyDatabase.deleteDBEntry tagEntry (tagKeyToKey (tagKey tag_p))

--- Gets a Tag entity stored in the database with the given key.
getTag :: TagKey -> KeyDatabase.Transaction Tag
getTag key = ERDGeneric.getEntry tagEntry keytuple2Tag (tagKeyToKey key)

--- Gets all Tag entities stored in the database.
queryAllTags :: KeyDatabase.Query [Tag]
queryAllTags =
  KeyDatabase.transformQ (map (uncurry keytuple2Tag))
   (KeyDatabase.allDBKeyInfos tagEntry)

--- Gets all Tag entities satisfying a given condition.
queryCondTag :: (Tag -> Bool) -> KeyDatabase.Query [Tag]
queryCondTag econd = KeyDatabase.transformQ (filter econd) queryAllTags

--- Database predicate representing the relation between keys and Comment tuple entities.
commentEntry :: ERDGeneric.Key -> CommentTuple -> KeyDatabase.Dynamic
commentEntry =
  KeyDatabase.persistentSQLite smapDB "Comment"
   ["Text","Date","UserCAuthoringKey","MetadataCommentingKey"]

--- Dynamic predicate representing the relation
--- between keys and Comment entities.
comment :: CommentKey -> Comment -> KeyDatabase.Dynamic
comment key obj
  | key =:= commentKey obj = commentEntry (commentKeyToKey key)
                              (comment2tuple obj)

--- Gets the key of a Comment entity.
commentKey :: Comment -> CommentKey
commentKey (Comment x _ _ _ _) = CommentKey x

--- Shows the key of a Comment entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCommentKey :: Comment -> String
showCommentKey obj =
  ERDGeneric.showDatabaseKey "Comment" commentKeyToKey (commentKey obj)

--- Transforms a string into a key of a Comment entity.
--- Nothing is returned if the string does not represent a reasonable key.
readCommentKey :: String -> Maybe CommentKey
readCommentKey s = ERDGeneric.readDatabaseKey "Comment" CommentKey s

commentKeyToKey :: CommentKey -> ERDGeneric.Key
commentKeyToKey (CommentKey k) = k

maybeCommentKeyToKey :: Maybe CommentKey -> Maybe ERDGeneric.Key
maybeCommentKeyToKey Nothing = Nothing
maybeCommentKeyToKey (Just (CommentKey k)) = Just k

--- Inserts a new Comment entity.
newCommentWithUserCAuthoringKeyWithMetadataCommentingKey
 :: String -> Time.CalendarTime -> UserKey -> MetadataKey
  -> KeyDatabase.Transaction Comment
newCommentWithUserCAuthoringKeyWithMetadataCommentingKey text_p date_p
                                                         userCAuthoringKey_p
                                                         metadataCommentingKey_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey userCAuthoringKey_p) |>>
   (ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
     (metadataKeyToKey metadataCommentingKey_p) |>>
    ERDGeneric.newEntry commentEntry keytuple2Comment
     (text_p,date_p,userKeyToKey userCAuthoringKey_p
     ,metadataKeyToKey metadataCommentingKey_p))

--- Updates an existing Comment entity.
updateComment :: Comment -> KeyDatabase.Transaction ()
updateComment comment_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey (commentUserCAuthoringKey comment_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
     (metadataKeyToKey (commentMetadataCommentingKey comment_p)) |>>
    KeyDatabase.updateDBEntry commentEntry
     (commentKeyToKey (commentKey comment_p)) (comment2tuple comment_p))

--- Deletes an existing Comment entity.
deleteComment :: Comment -> KeyDatabase.Transaction ()
deleteComment comment_p =
  KeyDatabase.deleteDBEntry commentEntry
   (commentKeyToKey (commentKey comment_p))

--- Gets a Comment entity stored in the database with the given key.
getComment :: CommentKey -> KeyDatabase.Transaction Comment
getComment key =
  ERDGeneric.getEntry commentEntry keytuple2Comment (commentKeyToKey key)

--- Gets all Comment entities stored in the database.
queryAllComments :: KeyDatabase.Query [Comment]
queryAllComments =
  KeyDatabase.transformQ (map (uncurry keytuple2Comment))
   (KeyDatabase.allDBKeyInfos commentEntry)

--- Gets all Comment entities satisfying a given condition.
queryCondComment :: (Comment -> Bool) -> KeyDatabase.Query [Comment]
queryCondComment econd =
  KeyDatabase.transformQ (filter econd) queryAllComments

--- Database predicate representing the relation between keys and Favoriting tuple entities.
favoritingEntry :: ERDGeneric.Key -> FavoritingTuple -> KeyDatabase.Dynamic
favoritingEntry =
  KeyDatabase.persistentSQLite smapDB "Favoriting"
   ["UserFavoritingKey","MetadataFavoritingKey"]

favoritingUserFavoritingKey :: Favoriting -> UserKey
favoritingUserFavoritingKey (Favoriting x _) = UserKey x

favoritingMetadataFavoritingKey :: Favoriting -> MetadataKey
favoritingMetadataFavoritingKey (Favoriting _ x) = MetadataKey x

--- Dynamic predicate representing the Favoriting relation between User entities and Metadata entities
favoriting :: UserKey -> MetadataKey -> KeyDatabase.Dynamic
favoriting (UserKey key1) (MetadataKey key2) =
  favoritingEntry unknown (key1,key2)

--- Inserts a new Favoriting relation between a User entity and a Metadata entity
newFavoriting :: UserKey -> MetadataKey -> KeyDatabase.Transaction ()
newFavoriting key1 key2 =
  ERDGeneric.existsEntryWithDBKey "User" userEntry (userKeyToKey key1) |>>
   (ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
     (metadataKeyToKey key2) |>>
    (ERDGeneric.unique2 favoritingEntry (userKeyToKey key1)
      (metadataKeyToKey key2) |>>
     ERDGeneric.newEntryR favoritingEntry (userKeyToKey key1)
      (metadataKeyToKey key2)))

--- Deletes an existing Favoriting relation between a User entity and a Metadata entity
deleteFavoriting :: UserKey -> MetadataKey -> KeyDatabase.Transaction ()
deleteFavoriting key1 key2 =
  ERDGeneric.deleteEntryR favoritingEntry (userKeyToKey key1)
   (metadataKeyToKey key2)

--- Gets the associated User entities for a given Metadata entity
getUserMetadatas :: User -> KeyDatabase.Transaction [Metadata]
getUserMetadatas e =
  let ekey = userKey e
   in KeyDatabase.getDB
       (queryCondFavoriting (\ t -> favoritingUserFavoritingKey t == ekey))
       |>>=
       (KeyDatabase.mapT getMetadata . map favoritingMetadataFavoritingKey)

--- Gets all Favoriting relationship entities stored in the database.
queryAllFavoritings :: KeyDatabase.Query [Favoriting]
queryAllFavoritings =
  KeyDatabase.transformQ (map (uncurry keytuple2Favoriting))
   (KeyDatabase.allDBKeyInfos favoritingEntry)

--- Gets all Favoriting relationship entities satisfying a given condition.
queryCondFavoriting :: (Favoriting -> Bool) -> KeyDatabase.Query [Favoriting]
queryCondFavoriting econd =
  KeyDatabase.transformQ (filter econd) queryAllFavoritings

--- Database predicate representing the relation between keys and Tagging tuple entities.
taggingEntry :: ERDGeneric.Key -> TaggingTuple -> KeyDatabase.Dynamic
taggingEntry =
  KeyDatabase.persistentSQLite smapDB "Tagging"
   ["MetadataTaggingKey","TagTaggingKey"]

taggingMetadataTaggingKey :: Tagging -> MetadataKey
taggingMetadataTaggingKey (Tagging x _) = MetadataKey x

taggingTagTaggingKey :: Tagging -> TagKey
taggingTagTaggingKey (Tagging _ x) = TagKey x

--- Dynamic predicate representing the Tagging relation between Metadata entities and Tag entities
tagging :: MetadataKey -> TagKey -> KeyDatabase.Dynamic
tagging (MetadataKey key1) (TagKey key2) = taggingEntry unknown (key1,key2)

--- Inserts a new Tagging relation between a Metadata entity and a Tag entity
newTagging :: MetadataKey -> TagKey -> KeyDatabase.Transaction ()
newTagging key1 key2 =
  ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
   (metadataKeyToKey key1) |>>
   (ERDGeneric.existsEntryWithDBKey "Tag" tagEntry (tagKeyToKey key2) |>>
    (ERDGeneric.unique2 taggingEntry (metadataKeyToKey key1)
      (tagKeyToKey key2) |>>
     ERDGeneric.newEntryR taggingEntry (metadataKeyToKey key1)
      (tagKeyToKey key2)))

--- Deletes an existing Tagging relation between a Metadata entity and a Tag entity
deleteTagging :: MetadataKey -> TagKey -> KeyDatabase.Transaction ()
deleteTagging key1 key2 =
  ERDGeneric.deleteEntryR taggingEntry (metadataKeyToKey key1)
   (tagKeyToKey key2)

--- Gets the associated Metadata entities for a given Tag entity
getMetadataTags :: Metadata -> KeyDatabase.Transaction [Tag]
getMetadataTags e =
  let ekey = metadataKey e
   in KeyDatabase.getDB
       (queryCondTagging (\ t -> taggingMetadataTaggingKey t == ekey)) |>>=
       (KeyDatabase.mapT getTag . map taggingTagTaggingKey)

--- Gets all Tagging relationship entities stored in the database.
queryAllTaggings :: KeyDatabase.Query [Tagging]
queryAllTaggings =
  KeyDatabase.transformQ (map (uncurry keytuple2Tagging))
   (KeyDatabase.allDBKeyInfos taggingEntry)

--- Gets all Tagging relationship entities satisfying a given condition.
queryCondTagging :: (Tagging -> Bool) -> KeyDatabase.Query [Tagging]
queryCondTagging econd =
  KeyDatabase.transformQ (filter econd) queryAllTaggings

--- Dynamic predicate representing the LangImpl relation
--- between Language entities and System entities.
langImpl :: LanguageKey -> SystemKey -> KeyDatabase.Dynamic
langImpl key1 key2
  | systemLanguageLangImplKey en =:= key1 = systemEntry (systemKeyToKey key2)
                                             (system2tuple en)
  where en free

--- Dynamic predicate representing role "hasLangImpl".
hasLangImpl :: LanguageKey -> SystemKey -> KeyDatabase.Dynamic
hasLangImpl = langImpl

--- Dynamic predicate representing role "hasLangImpl".
isALangImplOf :: SystemKey -> LanguageKey -> KeyDatabase.Dynamic
isALangImplOf = flip hasLangImpl

--- Dynamic predicate representing role "hasFavorite".
hasFavorite :: UserKey -> MetadataKey -> KeyDatabase.Dynamic
hasFavorite = favoriting

--- Dynamic predicate representing role "isAFavoriteOf".
isAFavoriteOf :: MetadataKey -> UserKey -> KeyDatabase.Dynamic
isAFavoriteOf = flip favoriting

--- Dynamic predicate representing the Commenting relation
--- between Metadata entities and Comment entities.
commenting :: MetadataKey -> CommentKey -> KeyDatabase.Dynamic
commenting key1 key2
  | commentMetadataCommentingKey en =:= key1 = commentEntry
                                                (commentKeyToKey key2)
                                                (comment2tuple en)
  where en free

--- Dynamic predicate representing role "hasComment".
hasComment :: MetadataKey -> CommentKey -> KeyDatabase.Dynamic
hasComment = commenting

--- Dynamic predicate representing role "hasComment".
isACommentOf :: CommentKey -> MetadataKey -> KeyDatabase.Dynamic
isACommentOf = flip hasComment

--- Dynamic predicate representing the CAuthoring relation
--- between User entities and Comment entities.
cAuthoring :: UserKey -> CommentKey -> KeyDatabase.Dynamic
cAuthoring key1 key2
  | commentUserCAuthoringKey en =:= key1 = commentEntry (commentKeyToKey key2)
                                            (comment2tuple en)
  where en free

--- Dynamic predicate representing role "isTheCAuthorOf".
isTheCAuthorOf :: UserKey -> CommentKey -> KeyDatabase.Dynamic
isTheCAuthorOf = cAuthoring

--- Dynamic predicate representing role "isTheCAuthorOf".
hasCAuthor :: CommentKey -> UserKey -> KeyDatabase.Dynamic
hasCAuthor = flip isTheCAuthorOf

--- Dynamic predicate representing role "hasTag".
hasTag :: MetadataKey -> TagKey -> KeyDatabase.Dynamic
hasTag = tagging

--- Dynamic predicate representing role "isATagOf".
isATagOf :: TagKey -> MetadataKey -> KeyDatabase.Dynamic
isATagOf = flip tagging

--- Dynamic predicate representing the Versioning relation
--- between Metadata entities and Version entities.
versioning :: MetadataKey -> VersionKey -> KeyDatabase.Dynamic
versioning key1 key2
  | versionMetadataVersioningKey en =:= key1 = versionEntry
                                                (versionKeyToKey key2)
                                                (version2tuple en)
  where en free

--- Dynamic predicate representing role "hasVersion".
hasVersion :: MetadataKey -> VersionKey -> KeyDatabase.Dynamic
hasVersion = versioning

--- Dynamic predicate representing role "hasVersion".
isAVersionOf :: VersionKey -> MetadataKey -> KeyDatabase.Dynamic
isAVersionOf = flip hasVersion

--- Dynamic predicate representing the Authoring relation
--- between Metadata entities and User entities.
authoring :: MetadataKey -> UserKey -> KeyDatabase.Dynamic
authoring key1 key2
  | metadataUserAuthoringKey en =:= key2 = metadataEntry
                                            (metadataKeyToKey key1)
                                            (metadata2tuple en)
  where en free

--- Dynamic predicate representing role "hasAuthor".
hasAuthor :: MetadataKey -> UserKey -> KeyDatabase.Dynamic
hasAuthor = authoring

--- Dynamic predicate representing role "hasAuthor".
isTheAuthorOf :: UserKey -> MetadataKey -> KeyDatabase.Dynamic
isTheAuthorOf = flip hasAuthor

--- Dynamic predicate representing the ImplLang relation
--- between Metadata entities and Language entities.
implLang :: MetadataKey -> LanguageKey -> KeyDatabase.Dynamic
implLang key1 key2
  | metadataLanguageImplLangKey en =:= key2 = metadataEntry
                                               (metadataKeyToKey key1)
                                               (metadata2tuple en)
  where en free

--- Dynamic predicate representing role "hasImplLang".
hasImplLang :: MetadataKey -> LanguageKey -> KeyDatabase.Dynamic
hasImplLang = implLang

--- Dynamic predicate representing role "hasImplLang".
isTheImplLangOf :: LanguageKey -> MetadataKey -> KeyDatabase.Dynamic
isTheImplLangOf = flip hasImplLang

--- Checks the consistency of the complete database.
checkAllData :: KeyDatabase.Transaction ()
checkAllData =
  checkFavoriting |>>
   (checkTagging |>>
    (checkMetadata |>>
     (checkVersion |>>
      (checkLanguage |>>
       (checkSystem |>> (checkUser |>> (checkTag |>> checkComment)))))))

--- Checks the consistency of the database for Favoriting entities.
checkFavoriting :: KeyDatabase.Transaction ()
checkFavoriting =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos favoritingEntry) |>>=
   (KeyDatabase.mapT_ checkFavoritingEntry .
    map (uncurry keytuple2Favoriting))

--- Checks the consistency of the database for Tagging entities.
checkTagging :: KeyDatabase.Transaction ()
checkTagging =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos taggingEntry) |>>=
   (KeyDatabase.mapT_ checkTaggingEntry . map (uncurry keytuple2Tagging))

--- Checks the consistency of the database for Metadata entities.
checkMetadata :: KeyDatabase.Transaction ()
checkMetadata =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos metadataEntry) |>>=
   (KeyDatabase.mapT_ checkMetadataEntry . map (uncurry keytuple2Metadata))

--- Checks the consistency of the database for Version entities.
checkVersion :: KeyDatabase.Transaction ()
checkVersion =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos versionEntry) |>>=
   (KeyDatabase.mapT_ checkVersionEntry . map (uncurry keytuple2Version))

--- Checks the consistency of the database for Language entities.
checkLanguage :: KeyDatabase.Transaction ()
checkLanguage =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos languageEntry) |>>=
   (KeyDatabase.mapT_ checkLanguageEntry . map (uncurry keytuple2Language))

--- Checks the consistency of the database for System entities.
checkSystem :: KeyDatabase.Transaction ()
checkSystem =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos systemEntry) |>>=
   (KeyDatabase.mapT_ checkSystemEntry . map (uncurry keytuple2System))

--- Checks the consistency of the database for User entities.
checkUser :: KeyDatabase.Transaction ()
checkUser =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos userEntry) |>>=
   (KeyDatabase.mapT_ checkUserEntry . map (uncurry keytuple2User))

--- Checks the consistency of the database for Tag entities.
checkTag :: KeyDatabase.Transaction ()
checkTag =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos tagEntry) |>>=
   (KeyDatabase.mapT_ checkTagEntry . map (uncurry keytuple2Tag))

--- Checks the consistency of the database for Comment entities.
checkComment :: KeyDatabase.Transaction ()
checkComment =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos commentEntry) |>>=
   (KeyDatabase.mapT_ checkCommentEntry . map (uncurry keytuple2Comment))

checkFavoritingEntry :: Favoriting -> KeyDatabase.Transaction ()
checkFavoritingEntry favoriting_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey (favoritingUserFavoritingKey favoriting_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
     (metadataKeyToKey (favoritingMetadataFavoritingKey favoriting_p)) |>>
    ERDGeneric.unique2C favoritingEntry
     (userKeyToKey (favoritingUserFavoritingKey favoriting_p))
     (metadataKeyToKey (favoritingMetadataFavoritingKey favoriting_p)))

checkTaggingEntry :: Tagging -> KeyDatabase.Transaction ()
checkTaggingEntry tagging_p =
  ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
   (metadataKeyToKey (taggingMetadataTaggingKey tagging_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "Tag" tagEntry
     (tagKeyToKey (taggingTagTaggingKey tagging_p)) |>>
    ERDGeneric.unique2C taggingEntry
     (metadataKeyToKey (taggingMetadataTaggingKey tagging_p))
     (tagKeyToKey (taggingTagTaggingKey tagging_p)))

checkMetadataEntry :: Metadata -> KeyDatabase.Transaction ()
checkMetadataEntry metadata_p =
  ERDGeneric.duplicateKeyTest metadataEntry |>>
   (ERDGeneric.existsEntryWithDBKey "Language" languageEntry
     (languageKeyToKey (metadataLanguageImplLangKey metadata_p)) |>>
    (ERDGeneric.existsEntryWithDBKey "User" userEntry
      (userKeyToKey (metadataUserAuthoringKey metadata_p)) |>>
     ERDGeneric.minTestC "Version" versionEntry keytuple2Version
      versionMetadataVersioningKey 1 (metadataKey metadata_p)))

checkVersionEntry :: Version -> KeyDatabase.Transaction ()
checkVersionEntry version_p =
  ERDGeneric.duplicateKeyTest versionEntry |>>
   ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
    (metadataKeyToKey (versionMetadataVersioningKey version_p))

checkLanguageEntry :: Language -> KeyDatabase.Transaction ()
checkLanguageEntry language_p =
  ERDGeneric.duplicateKeyTest languageEntry |>>
   (ERDGeneric.uniqueC "Smap" languageEntry keytuple2Language languageName
     language_p |>>
    (ERDGeneric.uniqueC "Smap" languageEntry keytuple2Language
      languageFilenameExt language_p |>>
     ERDGeneric.minTestC "System" systemEntry keytuple2System
      systemLanguageLangImplKey 1 (languageKey language_p)))

checkSystemEntry :: System -> KeyDatabase.Transaction ()
checkSystemEntry system_p =
  ERDGeneric.duplicateKeyTest systemEntry |>>
   (ERDGeneric.uniqueC "Smap" systemEntry keytuple2System systemName system_p
    |>>
    (ERDGeneric.uniqueC "Smap" systemEntry keytuple2System systemExecUrl
      system_p |>>
     ERDGeneric.existsEntryWithDBKey "Language" languageEntry
      (languageKeyToKey (systemLanguageLangImplKey system_p))))

checkUserEntry :: User -> KeyDatabase.Transaction ()
checkUserEntry user_p =
  ERDGeneric.duplicateKeyTest userEntry |>>
   (ERDGeneric.uniqueC "Smap" userEntry keytuple2User userName user_p |>>
    ERDGeneric.uniqueC "Smap" userEntry keytuple2User userEmail user_p)

checkTagEntry :: Tag -> KeyDatabase.Transaction ()
checkTagEntry tag_p =
  ERDGeneric.duplicateKeyTest tagEntry |>>
   ERDGeneric.uniqueC "Smap" tagEntry keytuple2Tag tagName tag_p

checkCommentEntry :: Comment -> KeyDatabase.Transaction ()
checkCommentEntry comment_p =
  ERDGeneric.duplicateKeyTest commentEntry |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey (commentUserCAuthoringKey comment_p)) |>>
    ERDGeneric.existsEntryWithDBKey "Metadata" metadataEntry
     (metadataKeyToKey (commentMetadataCommentingKey comment_p)))

--- Saves the complete database as Curry terms.
--- The first argument is the directory where the term files should be stored.
saveAllData :: String -> IO ()
saveAllData path =
  do ERDGeneric.saveDBTerms path "Metadata" metadataEntry keytuple2Metadata
     ERDGeneric.saveDBTerms path "Version" versionEntry keytuple2Version
     ERDGeneric.saveDBTerms path "Language" languageEntry keytuple2Language
     ERDGeneric.saveDBTerms path "System" systemEntry keytuple2System
     ERDGeneric.saveDBTerms path "User" userEntry keytuple2User
     ERDGeneric.saveDBTerms path "Tag" tagEntry keytuple2Tag
     ERDGeneric.saveDBTerms path "Comment" commentEntry keytuple2Comment
     ERDGeneric.saveDBTerms path "Favoriting" favoritingEntry
      keytuple2Favoriting
     ERDGeneric.saveDBTerms path "Tagging" taggingEntry keytuple2Tagging

--- Restore the complete database from files containing Curry terms.
--- The first argument is the directory where the term files are stored.
restoreAllData :: String -> IO ()
restoreAllData path =
  do ERDGeneric.restoreDBTerms path "Metadata" metadataEntry
      (metadataKeyToKey . metadataKey) metadata2tuple
     ERDGeneric.restoreDBTerms path "Version" versionEntry
      (versionKeyToKey . versionKey) version2tuple
     ERDGeneric.restoreDBTerms path "Language" languageEntry
      (languageKeyToKey . languageKey) language2tuple
     ERDGeneric.restoreDBTerms path "System" systemEntry
      (systemKeyToKey . systemKey) system2tuple
     ERDGeneric.restoreDBTerms path "User" userEntry (userKeyToKey . userKey)
      user2tuple
     ERDGeneric.restoreDBTerms path "Tag" tagEntry (tagKeyToKey . tagKey)
      tag2tuple
     ERDGeneric.restoreDBTerms path "Comment" commentEntry
      (commentKeyToKey . commentKey) comment2tuple
     ERDGeneric.restoreDBRelTerms path "Favoriting" favoritingEntry
      favoriting2tuple
     ERDGeneric.restoreDBRelTerms path "Tagging" taggingEntry tagging2tuple
