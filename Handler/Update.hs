{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Update where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (fromText, toText, encodeString)
import           Filesystem
import           Import hiding (object, FilePath)
import           System.Exit
import           System.IO (hGetContents)
import           System.Process
import           Text.Blaze
import           Yesod.Default.Config
import           Yesod.Json ()
import           Prelude hiding (FilePath)

data UpdateStatus = UpdateStatus
    { name    :: T.Text
    , success :: Bool
    }

instance ToJSON UpdateStatus where
    toJSON (UpdateStatus n s) =
        object [ "name"    .= n
               , "success" .= s
               ]

getUpdateR :: T.Text -> Handler RepHtml
getUpdateR repoName = do
    (Extra {..}) <- appExtra . settings <$> getYesod
    if repoName `elem` extraRepos
        then defaultLayout $ updateWidget (fromText extraBaseDir)
                                          (fromText repoName)
        else notFound

instance ToMarkup FilePath where
    toMarkup = toMarkup . encodeString

instance ToMarkup ExitCode where
    toMarkup = toMarkup . show

updateWidget :: FilePath -> FilePath -> Widget
updateWidget baseDir repoName = do
    setTitle . toHtml $ T.concat ["Update ", either id id $ toText repoName]
    (_, mout, _, pr) <- liftIO . createProcess $
          (proc "git" ["pull"]) { cwd = Just $ encodeString repoDir }
    ec  <- liftIO $ waitForProcess pr
    out <- liftIO $ maybe (return "<no output>") hGetContents mout
    $(widgetFile "update")
    where
        repoDir = baseDir </> repoName

