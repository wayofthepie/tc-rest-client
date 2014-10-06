{-# LANGUAGE DeriveGeneric #-}
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


-- | Reference to the parent project
data ParentProjectRef = ParentProjectRef {
        _pprId          :: Text,
        _pprName        :: Text,
        _pprDescription :: Maybe Text,
        _pprHref        :: Text,
        _pprWebUrl      :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON ParentProjectRef

instance Reference ParentProjectRef Project where
    follow ppr = liftM ( eitherDecode . responseBody ) $
        makeRequest (APIRequestGet $ _pprHref ppr) >>= getResponse



-- | Information on build types
data BuildConfigs = BuildConfigs {
        _bTypesCount    :: Int,
        _bTypes         :: [BuildConfigRef]
    } deriving (Eq, Show, Generic)

instance FromJSON BuildConfigs



data Templates = Templates {
        _templatesCount     :: Int,
        _templatesBuildConfigs:: [BuildConfigRef]
    } deriving (Eq, Show, Generic)

instance FromJSON Templates


-- | Reference to a build type
data BuildConfigRef = BuildConfigRef {
        _bTypeRefId         :: Text,
        _bTypeRefName       :: Text,
        _bTypeRefProjectName:: Text,
        _bTypeRefProjectId  :: Text,
        _bTypeRefHref       :: Text,
        _bTypeRefWebUrl     :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON BuildConfigRef



-- | Reference to a parameter
data ParametersRef = ParametersRef {
        _paramsRefCount      :: Int,
        _paramsRefHref       :: Text,
        _paramsRefProperties :: [Property]
    } deriving (Eq, Show, Generic)

instance FromJSON ParametersRef



-- | A build property
data Property = Property {
        _propName   :: Text,
        _propValue  :: Text,
        _propOwn    :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance FromJSON Property


-- | Reference to the vcs roots
data VcsRootsRef = VcsRootsRef {
        _vcsRootsRefHref    :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON VcsRootsRef



data SubProjectRefs = SubProjectRefs {
        _subProjectsCount   :: Int,
        _subProjectsRefs    :: Maybe [ProjectRef]
    } deriving (Eq, Show, Generic)

instance FromJSON SubProjectRefs


data ProjectRef = ProjectRef {
        _projectRefId       :: Text,
        _projectRefName     :: Text,
        _projectRefParentId :: Text,
        _projectRefHref     :: Text,
        _projectRefWebUrl   :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON ProjectRef


-- | A project
data Project = Project {
        _projectid              :: Text,
        _projectName            :: Text,
        _projectParentId        :: Text,
        _projectDesc            :: Maybe Text,
        _projectHref            :: Text,
        _projectWebUrl          :: Text,
        _projectParentRef       :: ParentProjectRef,
        _projectBuildConfigs    :: BuildConfigs,
        _projectTemplates       :: Templates,
        _projectParams          :: ParametersRef,
        _projectVcsRoots        :: VcsRootsRef,
        _projectSubProjectRefs  :: SubProjectRefs
    } deriving (Eq, Show, Generic)

instance FromJSON Project



data BuildConfig = BuildConfig {
       _buildConfigId           :: Text,
       _buildConfigTypeId       :: Text,
       _buildConfigNumber       :: Int,
       _buildConfigStatus       :: Text,
       _buildConfigState        :: Text,
       _buildConfigHref         :: Text,
       _buildConfigWebUrl       :: Text,
       _buildConfigStatusText   :: Text,
       _buildConfigBuildRef     :: BuildConfigRef, -- Self reference
       _buildConfigTags         :: Maybe Object,
       _buildConfigQueuedDate   :: Text, -- Should parse this to a proper type
       _buildConfigStartDate    :: Text, -- Should parse this to a proper type
       _buildConfigFinishDate   :: Text, -- Should parse this to a proper type
       _buildConfigTriggered    :: Maybe Object,-- BuildTriggeredInfo
       _buildConfigLastChanges  :: Maybe Object, -- ???
       _buildConfigChanges      :: Maybe Object, -- ???
       _buildConfigRevisions    :: Maybe Object, -- ???
       _buildConfigAgent        :: Maybe Object, -- BuildAgentRef
       _buildConfigArtifacts    :: Maybe Object, -- BuildArtifactsRef
       _buildConfigRelatedIssues:: Maybe Object,
       _buildConfigStatistics   :: Maybe Object
    } deriving (Eq, Show, Generic)

instance FromJSON BuildConfig


-- | Run a request.
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp

-- | Some hacking around....
projects ::
    ( MonadIO m, MonadThrow m, MonadBaseControl IO m ) => m ( Maybe Array )
projects = liftM ( (\x -> x ^? key "/httpAuth/app/rest/project" . _Array) . responseBody ) $
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

