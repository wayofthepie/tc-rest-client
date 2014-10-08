{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Aeson
import Data.Aeson.Lens

-- applyBasicAuth expects a strict ByteString
import Data.ByteString.Lazy.Char8 hiding (filter, foldl)

import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Text as T hiding (foldl)
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
    follow :: ( MonadBaseControl IO m, MonadIO m, MonadThrow m ) =>
        a -> m b

-- Only use GET for now for all follows

instance Reference ParentProjectRef ( Either String Project ) where
    follow = follow' _pprHref

instance Reference BuildRunRef ( Either String BuildRun ) where
    follow = follow' _buildRunRefHref

instance Reference ParametersRef ( Either String Parameters ) where
    follow = follow' _paramsRefHref

instance Reference VcsRootsRef ( Either String VcsRoots ) where
    follow = follow' _vcsRootsRefHref

instance Reference ProjectRef ( Either String Project ) where
    follow = follow' _projectRefHref

instance Reference SubProjectRefs [Either String Project] where
    follow spr
        | _subProjectRefs spr == Nothing = return []
        | otherwise = mapM (\r -> follow' _projectRefHref r) $
            fromMaybe [] subProjRefs
        where
            subProjRefs :: Maybe [ProjectRef]
            subProjRefs = _subProjectRefs spr

-- | Apply __f__ to __ref__ and request
follow' :: ( MonadBaseControl IO m, MonadIO m, MonadThrow m, FromJSON b ) =>
    ( a -> Text ) -> a -> m (Either String b)
follow' f ref =  liftM ( eitherDecode . responseBody ) $
    makeRequest (APIRequestGet $ f ref) >>= getResponse


-- | Run a request.
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp


-- | Some hacking around....
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

