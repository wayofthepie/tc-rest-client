{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Client where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

-- applyBasicAuth expects a strict ByteString
import Data.ByteString.Lazy.Char8 hiding (filter)

import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Network.HTTP.Conduit
import System.Environment


--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

data APIRequest  =
    APIRequestPost {
        _endpoint :: String
    }
    | APIRequestGet {
        _endpoint :: String
    }
    | APIRequestPut  {
        _endpoint :: String
    }
    | APIRequestDelete {
        _endpoint :: String
    }


-- | Build a request of the specified APIRequest type.
makeRequest :: (MonadIO m, MonadThrow m) => APIRequest -> m Request
makeRequest (APIRequestPost endpoint) = requestBuilder endpoint postReq
    where postReq req = req { method = "POST" }

makeRequest (APIRequestGet endpoint) = requestBuilder endpoint getReq
    where getReq req = req { method = "GET" }

makeRequest (APIRequestPut endpoint) = requestBuilder endpoint putReq
    where putReq req = req { method = "PUT" }

makeRequest (APIRequestDelete endpoint) = requestBuilder endpoint deleteReq
    where deleteReq req = req { method = "DELETE" }


-- | Build a request with basic authentication.
requestBuilder ::
    ( MonadIO m, MonadThrow m ) => String -> (Request -> Request) -> m Request
requestBuilder endpoint httpMethod = apibase >>=
        \url -> ( parseUrl $ url ++ endpoint ) >>=
            addBasicAuth . acceptJson . httpMethod

    where addBasicAuth :: MonadIO m => Request -> m Request
          addBasicAuth req = tcCredentials >>=
                \(u, p) -> return $ applyBasicAuth (BC.pack u) (BC.pack p) req

          acceptJson :: Request -> Request
          acceptJson req =
            req { requestHeaders = [("Accept", "application/json")] }
--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

-- | Run a request.
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response ByteString )
getResponse req = withManager $ \manager -> do
    resp <-  httpLbs req manager
    return resp

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


apibase :: MonadIO m => m String
apibase = liftM (++ apiUrl) $ baseUrl
    where
        -- Base url for all API calls.
        -- Assumes http. Retrieves the hostname (or IP address)
        -- from an environment variable called "TEAMCITY_HOST".
        baseUrl :: MonadIO m => m String
        baseUrl =  liftIO $ liftM ("http://" ++ ) $ getEnv "TEAMCITY_HOST"
        -- Hard code this for now
        apiUrl :: String
        apiUrl = "/httpAuth/app/rest/"

