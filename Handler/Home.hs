{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Home where

import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           Control.Exception (bracket_)
import           Control.Monad
import           Import hiding (FilePath)
import           Yesod.Default.Config
-- import           Yesod.Logger
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (fromText, encodeString)
import           Filesystem (getWorkingDirectory, setWorkingDirectory)
import           System.IO
import           System.Process
import           Text.Printf
import           Prelude hiding (FilePath)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (Extra {..}) <- appExtra . settings <$> getYesod
    aDomId <- newIdent
    defaultLayout $ do
        setTitle "Neatline Update"
        $(widgetFile "homepage")
        forM_ extraRepos (repoWidget extraBaseDir)

runInDir :: String -> String -> [String] -> IO (Maybe String)
runInDir dir exec args = do
    (_, mout, _, p) <- createProcess $ (proc exec args) { cwd     = Just dir
                                                        , std_out = CreatePipe
                                                        }
    ec  <- waitForProcess p

    case mout of
        Just h  -> Just <$> hGetContents h
        Nothing -> return Nothing

-- This is a view widget for displaying information about a repository.
repoWidget :: T.Text -> T.Text -> Widget
repoWidget baseDir name = do
    -- git branch | grep '*' | cut -f 2 -d ' '
    -- logger <- liftIO defaultDevelopmentLogger
    -- liftIO $ logText logger baseDir
    -- liftIO $ logText logger name
    -- liftIO $ runInDir baseDir (logString logger =<< readProcess "git" ["branch"] "")
    branches <- liftIO $ runInDir repoDir "git" ["branch"]
    -- liftIO . logString logger $ show branch
    let branch  = getCurrentBranch branches
        branch' = maybe "master" id branch
    _ <- liftIO $ runInDir repoDir "git" ["fetch", "--quiet"]
    behind <- liftIO $ maybe (-1) getWhatChanged <$> runInDir repoDir "git" [ "whatchanged", "--oneline"
                                                               , printf "%s..origin/%s" branch' branch'
                                                               ]
    $(widgetFile "repoView")
    where startsWith :: Char -> String -> Bool
          startsWith c (s:_) | c == s    = True
                             | otherwise = False
          startsWith _ _                 = False

          baseDir' = fromText baseDir
          name'    = fromText name
          repoDir' = baseDir' </> name'
          repoDir  = encodeString repoDir'

          getCurrentBranch :: Maybe String -> Maybe String
          getCurrentBranch output =   join
                                  $   ( listToMaybe
                                      . map (drop 2)
                                      . filter (elem '*')
                                      . lines
                                      )
                                  <$> output
          getWhatChanged :: String -> Int
          getWhatChanged = length . filter (not . startsWith ':') . lines

