{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Aeson
import Data.Aeson.Lens

-- applyBasicAuth expects a strict ByteString
import Data.ByteString.Lazy.Char8 hiding (filter)

import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Text as T
import Network.HTTP.Conduit
import System.Environment

import TeamCity.JSON.Types

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
-- Response
--------------------------------------------------------------------------------

class Reference a b where
    follow :: ( MonadIO m, MonadThrow m, MonadBaseControl IO m ) =>
        a -> m (Either String b)


instance Reference ParentProjectRef Project where
    follow ppr = liftM ( eitherDecode . responseBody ) $
        makeRequest (APIRequestGet $ _pprHref ppr) >>= getResponse




-- | Run a request.
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp


-- | Some hacking around....
projects ::
    ( MonadIO m, MonadThrow m, MonadBaseControl IO m ) => m ( Maybe Array )
projects = liftM ( (\x -> x ^? key "project" . _Array) . responseBody ) $
        makeRequest (APIRequestGet "/httpAuth/app/rest/projects") >>= getResponse

project :: ( MonadIO m, MonadThrow m, MonadBaseControl IO m ) =>
    Value -> m ( Maybe Object )
project projectUrl = liftM (\x -> fromJust x ^? ix 0 . _Object) projects


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
apibase = baseUrl
    where
        -- Base url for all API calls.
        -- Assumes http. Retrieves the hostname (or IP address)
        -- from an environment variable called "TEAMCITY_HOST".
        baseUrl :: MonadIO m => m String
        baseUrl =  liftIO $ liftM ("http://" ++ ) $ getEnv "TEAMCITY_HOST"
        -- Hard code this for now
        apiUrl :: String
        apiUrl = "/httpAuth/app/rest/"

