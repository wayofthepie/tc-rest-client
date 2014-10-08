{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TeamCity.Request where

import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Text as T hiding (foldl)
import GHC.Generics
import Network.HTTP.Conduit
import System.Environment


--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

data APIRequest  =
    APIRequestPost {
        _endpoint :: Text
    }
    | APIRequestGet {
        _endpoint :: Text
    }
    | APIRequestPut  {
        _endpoint :: Text
    }
    | APIRequestDelete {
        _endpoint :: Text
    }


-- | Build a request of the specified APIRequest type.
mkRequest :: (MonadIO m, MonadThrow m) => APIRequest -> m Request
mkRequest (APIRequestPost endpoint) = requestBuilder endpoint postReq
    where postReq req = req { method = "POST" }

mkRequest (APIRequestGet endpoint) = requestBuilder endpoint getReq
    where getReq req = req { method = "GET" }

mkRequest (APIRequestPut endpoint) = requestBuilder endpoint putReq
    where putReq req = req { method = "PUT" }

mkRequest (APIRequestDelete endpoint) = requestBuilder endpoint deleteReq
    where deleteReq req = req { method = "DELETE" }



-- | Build a request with basic authentication.
requestBuilder ::
    ( MonadIO m, MonadThrow m ) => Text -> (Request -> Request) -> m Request
requestBuilder endpoint httpMethod = apibase >>=
        \url -> ( parseUrl $ url ++ ( T.unpack endpoint ) ) >>=
            addBasicAuth . acceptJson . httpMethod

    where addBasicAuth :: MonadIO m => Request -> m Request
          addBasicAuth req = tcCredentials >>=
                \(u, p) -> return $ applyBasicAuth (BC.pack u) (BC.pack p) req

          acceptJson :: Request -> Request
          acceptJson req =
            req { requestHeaders = [("Accept", "application/json")] }


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Retrieve teamcity credentials.
--
-- Currently these are pulled from
-- environment variables - horribly insecure, but will do for now!
tcCredentials :: MonadIO m => m ( String, String )
tcCredentials = liftIO $ do
    user <- getEnv "TEAMCITY_USER"
    pass <- getEnv "TEAMCITY_PASSWORD"
    return $ (user, pass)


-- | Base url for all API calls.
-- Assumes http. Retrieves the hostname (or IP address)
-- from an environment variable called "TEAMCITY_HOST".
apibase :: MonadIO m => m String
apibase =  liftIO $ liftM ("http://" ++ ) $ getEnv "TEAMCITY_HOST"
        -- Hard code this for now

