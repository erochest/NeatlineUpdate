{-# LANGUAGE OverloadedStrings #-}

module Handler.Update where

import           Data.Aeson
import qualified Data.Text as T
import           Import hiding (object)
import           Yesod.Json ()

data UpdateStatus = UpdateStatus
    { name    :: T.Text
    , success :: Bool
    }

instance ToJSON UpdateStatus where
    toJSON (UpdateStatus n s) =
        object [ "name"    .= n
               , "success" .= s
               ]

postUpdateR :: T.Text -> Handler RepJson
postUpdateR repoName =
    jsonToRepJson . encode $ UpdateStatus repoName False

