--------------------------------------------------------------------------------
--- The module defines operations related to execution environments, languages
--- and systems where an execution environment is a pair containing a language
--- and a list of associated systems.
---
--- @author Lasse Kristopher Meyer
--- @version November 2018
--------------------------------------------------------------------------------

module Model.ExecEnv (
  ExecEnv,Model.Smap.Language,Model.Smap.System,Model.Smap.languageKey,
  Model.Smap.languageName,Model.Smap.languageFilenameExt,
  Model.Smap.languageTemplate,Model.Smap.systemKey,Model.Smap.systemName,
  Model.Smap.systemExecUrl,Model.Smap.showLanguageKey,Model.Smap.showSystemKey,
  getExecEnvByLanguageKey,getExecEnvByLanguageName,getAllExecEnvs,
  createLanguage,getAllLanguages,createSystem
) where

import Char
import KeyDatabase
import Maybe

import Model.Smap

--------------------------------------------------------------------------------
-- Type synonym for execution environments                                    --
--------------------------------------------------------------------------------

--- An execution environment is a pair containing
--- - a language
--- - a list of associated systems.
--- The motivation behind this is that in the context of execution (especially
--- in the SmapIE application component) languages must always be linked to
--- their implementations. Thus, in these situations, it is practicable to work
--- with a type synonym combining both entities.
type ExecEnv = (Language,[System])

--------------------------------------------------------------------------------
-- Reading execution environments from the database                           --
--------------------------------------------------------------------------------

--- Returns the execution environment for a given language key from the
--- database.
--- @param langKey - the key of the identifying language
getExecEnvByLanguageKey :: LanguageKey -> IO (Maybe ExecEnv)
getExecEnvByLanguageKey langKey =
  do tResult <- runT $ getLanguage langKey
     either (\lang -> do systems <- runQ $ queryCondSystem cond
                         return $ Just (lang,systems))
            (\_ -> return Nothing)
            tResult
  where
    cond = (langKey==) . systemLanguageLangImplKey

--- Returns the execution environment for a given language name from the
--- database. 
--- @param langName - the name of the identifying language
getExecEnvByLanguageName :: String -> IO (Maybe ExecEnv)
getExecEnvByLanguageName langName =
  do mLang <- runQ $ queryCondLanguage cond1
     maybe (return Nothing)
           (\lang -> do systems <- runQ $ queryCondSystem (cond2 lang)
                        return $ Just (lang,systems))
           (listToMaybe mLang)
  where 
    cond1 = (map toLower langName==) . map toLower . languageName
    cond2 lang = (languageKey lang==) . systemLanguageLangImplKey

--- Returns all languages with their associated systems from the database.
getAllExecEnvs :: IO [ExecEnv]
getAllExecEnvs = 
  do langs   <- runQ queryAllLanguages 
     systems <- runQ queryAllSystems
     return $ map (\lang -> (lang,filter (cond lang) systems)) langs
  where cond :: Language -> System -> Bool
        cond lang = (languageKey lang==) . systemLanguageLangImplKey

--------------------------------------------------------------------------------
-- Operations on languages and systems                                        --
--------------------------------------------------------------------------------

--- Creates a new language entity.
--- @param langData - name, filename extension and code template
createLanguage :: (String,String,String) -> IO (Either Language TError)
createLanguage (name,filenameExt,template) =
  runT $ newLanguage name filenameExt template

--- Returns all persisted languages.
getAllLanguages :: IO [Language]
getAllLanguages = runQ $ queryAllLanguages

--- Creates a new system entity.
--- @param systemData - name, execution URL and associated language
createSystem :: (String,String,Language) -> IO (Either System TError)
createSystem (name,execUrl,lang) =
  runT $ newSystemWithLanguageLangImplKey name execUrl (languageKey lang)

--------------------------------------------------------------------------------