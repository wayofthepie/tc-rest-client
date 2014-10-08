{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TeamCity.Response where

import Control.Arrow
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 hiding (map, pack, zipWith)
import Data.Maybe
import Data.Text as T hiding (foldl, map, zipWith)
import GHC.Generics
import Network.HTTP.Conduit

import TeamCity.Request
import TeamCity.JSON.Types


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
    mkRequest (APIRequestGet $ f ref) >>= getResponse


-- | Convenience function to call an endpoint.
--
-- TODO : Really have to think about how to model calls, especially seeing
-- as the base of most (all?) API calls is /httpAuth/app/rest/.
callep :: ( MonadBaseControl IO m,  MonadIO m, MonadThrow m ) =>
    Text -> m ( Response ByteString )
callep ep = getResponse =<< mkRequest (APIRequestGet $ T.append "/httpAuth/app/rest/" ep)

-- | Run a request.
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp


data Projects = Projects {
        count   :: Int,
        project :: [Object]
    } deriving (Eq, Show, Generic)

instance FromJSON Projects


-- | Get a list of project names and their location.
-- This method has a contract with the API in that the API will always
-- return __String__ values in the data this method asks for.
--
-- TODO: There must be a way of combining lenses to get the names and hrefs
-- simultaneously.
listProjects ::
    ( MonadBaseControl IO m, MonadIO m, MonadThrow m ) => m [(Text, Text)]
listProjects =
    let extractText (String x) = x
        names json      = json ^.. key "project" . values . key "href"
        hrefs json      = json ^.. key "project" . values . key "name"
        nameRefs json   = zipWith (,) (names json) (hrefs json)
        convert         = map (mapTuple extractText)
    in liftM (convert . nameRefs . responseBody) $ callep "projects"


-- | Get a project by id.
getProjectById :: ( MonadBaseControl IO m, MonadIO m, MonadThrow m ) =>
    Text -> m ( Either String Project )
getProjectById pid = liftM (eitherDecode . responseBody) $
    callep $ T.append "projects/id:" pid


-- | Map a function on both elements of a tuple
mapTuple :: Arrow a => a b c -> a (b, b) (c, c)
mapTuple = join (***)
