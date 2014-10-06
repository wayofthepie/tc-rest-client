{-# LANGUAGE OverloadedStrings #-}

module TeamCity.JSON.Types where
import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Text as T

-- | Reference to the parent project
data ParentProjectRef = ParentProjectRef {
        _pprId          :: Text,
        _pprName        :: Text,
        _pprDescription :: Maybe Text,
        _pprHref        :: Text,
        _pprWebUrl      :: Text
    } deriving (Eq, Show)

instance FromJSON ParentProjectRef where
    parseJSON (Object v) =
        ParentProjectRef <$> v .: "id"
                         <*> v .: "name"
                         <*> v .:? "description"
                         <*> v .: "href"
                         <*> v .: "webUrl"

-- | Information on build types
data BuildRuns = BuildRuns {
        _bTypesCount    :: Int,
        _bTypes         :: [BuildRunRef]
    } deriving (Eq, Show)

instance FromJSON BuildRuns where
    parseJSON (Object v) =
        BuildRuns <$> v .: "count"
                   <*> v .: "buildType"



data Templates = Templates {
        _templatesCount     :: Int,
        _templatesBuildRuns:: [BuildRunRef]
    } deriving (Eq, Show)

instance FromJSON Templates where
    parseJSON (Object v) =
        Templates <$> v .: "count"
                  <*> v .: "buildType"



-- | Reference to a build type
data BuildRunRef = BuildRunRef {
        _bTypeRefId         :: Text,
        _bTypeRefName       :: Text,
        _bTypeRefProjectName:: Text,
        _bTypeRefProjectId  :: Text,
        _bTypeRefHref       :: Text,
        _bTypeRefWebUrl     :: Text
    } deriving (Eq, Show)

instance FromJSON BuildRunRef where
    parseJSON (Object v) =
        BuildRunRef <$> v .: "id"
                     <*> v .: "name"
                     <*> v .: "projectName"
                     <*> v .: "projectId"
                     <*> v .: "href"
                     <*> v .: "webUrl"

--instance Reference BuildRunRef BuildRun where


-- | Reference to a parameter
data ParametersRef = ParametersRef {
        _paramsRefCount      :: Int,
        _paramsRefHref       :: Text,
        _paramsRefProperties :: [Property]
    } deriving (Eq, Show)

instance FromJSON ParametersRef where
    parseJSON (Object v) =
        ParametersRef <$> v .: "count"
                      <*> v .: "href"
                      <*> v .: "property"



-- | A build property
data Property = Property {
        _propName   :: Text,
        _propValue  :: Text,
        _propOwn    :: Maybe Bool
    } deriving (Eq, Show)

instance FromJSON Property where
    parseJSON (Object v) =
        Property <$> v .: "name"
                 <*> v .: "value"
                 <*> v .:? "own"



-- | Reference to the vcs roots
data VcsRootsRef = VcsRootsRef {
        _vcsRootsRefHref    :: Text
    } deriving (Eq, Show)

instance FromJSON VcsRootsRef where
    parseJSON (Object v) =
        VcsRootsRef <$> v .: "href"



data SubProjectRefs = SubProjectRefs {
        _subProjectsCount   :: Int,
        _subProjectsRefs    :: Maybe [ProjectRef]
    } deriving (Eq, Show)

instance FromJSON SubProjectRefs where
    parseJSON (Object v) =
        SubProjectRefs <$> v .: "count"
                    <*> v .:? "project"


data ProjectRef = ProjectRef {
        _projectRefId       :: Text,
        _projectRefName     :: Text,
        _projectRefParentId :: Text,
        _projectRefHref     :: Text,
        _projectRefWebUrl   :: Text
    } deriving (Eq, Show)

instance FromJSON ProjectRef where
    parseJSON (Object v) =
        ProjectRef <$> v .: "id"
                   <*> v .: "name"
                   <*> v .: "parentProjectId"
                   <*> v .: "href"
                   <*> v .: "webUrl"



-- | A project
data Project = Project {
        _projectid              :: Text,
        _projectName            :: Text,
        _projectParentId        :: Text,
        _projectDesc            :: Maybe Text,
        _projectHref            :: Text,
        _projectWebUrl          :: Text,
        _projectParentRef       :: ParentProjectRef,
        _projectBuildRuns       :: BuildRuns,
        _projectTemplates       :: Templates,
        _projectParams          :: ParametersRef,
        _projectVcsRoots        :: VcsRootsRef,
        _projectSubProjectRefs  :: SubProjectRefs
    } deriving (Eq, Show)

instance FromJSON Project where
   parseJSON (Object v) =
       Project <$> v .: "id"
               <*> v .: "name"
               <*> v .: "parentProjectId"
               <*> v .:? "description"
               <*> v .: "href"
               <*> v .: "webUrl"
               <*> v .: "parentProject"
               <*> v .: "buildTypes"
               <*> v .: "templates"
               <*> v .: "parameters"
               <*> v .: "vcsRoots"
               <*> v .: "projects"



data BuildRun = BuildRun {
       _buildRunId              :: Int,
       _buildRunTypeId          :: Text,
       _buildRunNumber          :: Text,
       _buildRunStatus          :: Text,
       _buildRunState           :: Text,
       _buildRunHref            :: Text,
       _buildRunWebUrl          :: Text,
       _buildRunStatusText      :: Text,
       _buildRunBuildRef        :: BuildRunRef, -- Self reference
       _buildRunTags            :: Maybe Object,
       _buildRunQueuedDate      :: Text, -- Should parse this to a proper type
       _buildRunStartDate       :: Text, -- Should parse this to a proper type
       _buildRunFinishDate      :: Text, -- Should parse this to a proper type
       _buildRunTriggered       :: Maybe Object,-- BuildTriggeredInfo
       _buildRunLastChanges     :: Maybe Object, -- ???
       _buildRunChanges         :: Maybe Object, -- ???
       _buildRunRevisions       :: Maybe Object, -- ???
       _buildRunAgent           :: Maybe Object, -- BuildAgentRef
       _buildRunArtifacts       :: Maybe Object, -- BuildArtifactsRef
       _buildRunRelatedIssue    :: Maybe Object,
       _buildRunStatistics      :: Maybe Object
    } deriving (Eq, Show)

instance FromJSON BuildRun where
    parseJSON (Object v) =
        BuildRun <$> v .: "id"
                    <*> v .: "buildTypeId"
                    <*> v .: "number"
                    <*> v .: "status"
                    <*> v .: "state"
                    <*> v .: "href"
                    <*> v .: "webUrl"
                    <*> v .: "statusText"
                    <*> v .: "buildType"
                    <*> v .: "tags"
                    <*> v .: "queuedDate"
                    <*> v .: "startDate"
                    <*> v .: "finishDate"
                    <*> v .: "triggered"
                    <*> v .: "lastChanges"
                    <*> v .: "changes"
                    <*> v .: "revisions"
                    <*> v .: "agent"
                    <*> v .: "artifacts"
                    <*> v .: "relatedIssues"
                    <*> v .: "statistics"

