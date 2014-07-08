--------------------------------------------------------------------------------
--- This module provides a controller to download programs.
---
--- @author Michael Hanus
--- @version July 2014
--------------------------------------------------------------------------------

module DownloadController (
  downloadController
) where

import ProgramModel
import HTML
import Authorization
import AuthorizedOperations
import Controllers
import Url
import SmapIEView(removeCRs)

------------------------------------------------------------------------------
-- Download controller                                                      --
------------------------------------------------------------------------------

--- The download controller delegates its work to the `showDownloadPage`
--- controller.
downloadController :: Url -> IO HtmlForm
downloadController url@(path,_) = 
  case path of
    ["download",progKey]         ->
        maybe (showInvalidUrlErrorPage url >>= getForm)
              (\k -> showDownloadPage k >>= toAnswer)
              (readProgramKeyAndVersionNumber (progKey,"0"))
    ["download",progKey,versNum] ->
        maybe (showInvalidUrlErrorPage url >>= getForm)
              (\k -> showDownloadPage k >>= toAnswer)
              (readProgramKeyAndVersionNumber (progKey,versNum))
    _ -> showInvalidUrlErrorPage url >>= getForm
 where
  toAnswer hexps = case hexps of
    [HtmlText s] -> return $ HtmlAnswer "text/plain" (removeCRs s)
    _ -> getForm hexps

-- Shows a specific version of an existing program as HtmlText. Returns
-- an error page if no program exists for the given key or if the version
-- number is not valid.
-- @param ctrlData - program key and version number
showDownloadPage :: (ProgramKey,Int) -> Controller
showDownloadPage (progKey,versNum) =
  do mProg <- getProgramByKey progKey
     maybe (showStdErrorPage programNotFoundErr) (\prog ->
           checkAuthorization (browserOperation $ ShowProgram prog) $ \_ ->
           do let mValidVersNum = validVersionNumber
                                    (length $ programVersions prog)
              maybe (showStdErrorPage $ versionNotFoundErr prog)
                    (\validVersNum -> return $ downloadPage (prog,validVersNum))
                    mValidVersNum)
           mProg
  where
    validVersionNumber versCount =
      if versNum == 0 -- always points to latest version
      then Just versCount
      else if versNum < 0 || versNum > versCount
           then Nothing
           else Just versNum

    programNotFoundErr =
      "We couldn't find the program you were looking for. It might have been "++
      "deleted by its author or probably never existed."

    versionNotFoundErr prog =
      "We couldn't find the version you requested (see <a href=?browser/"++
      showProgramKey prog++">latest version</a> instead)."

    downloadPage (prog,versnum) =
      [HtmlText (versionSourceCode ((programVersions prog) !! (versnum-1)))]

-------------------------------------------------------------------------------