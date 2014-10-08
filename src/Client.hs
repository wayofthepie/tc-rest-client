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



--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------


-- | Some hacking around....

