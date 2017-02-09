{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Types.Server
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types specific to the HTTP server and Pact service.
--
module Pact.Types.Server
--  , InboundPactChan(..),OutboundPactChan(..)
--  , initChans,writeInbound,writeOutbound,readInbound,tryReadOutbound,cmdToRequestKey
--  ) where
  where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.Maybe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.Text.Encoding
import Prelude hiding (log,exp)

import Pact.Pure
import Pact.Types.Runtime as Pact
import Pact.Types.Orphans ()
import Pact.Types.SQLite
import Pact.Types.Command

userSigToPactPubKey :: UserSig -> Pact.PublicKey
userSigToPactPubKey UserSig{..} = Pact.PublicKey $ encodeUtf8 _usPubKey

userSigsToPactKeySet :: [UserSig] -> S.Set Pact.PublicKey
userSigsToPactKeySet = S.fromList . fmap userSigToPactPubKey


data CommandConfig = CommandConfig {
      _ccDbFile :: Maybe FilePath
    , _ccDebugFn :: String -> IO ()
    , _ccEntity :: String
    }
$(makeLenses ''CommandConfig)

data CommandState = CommandState {
     _csRefStore :: RefStore
    }
$(makeLenses ''CommandState)

data ExecutionMode =
    Transactional { _emTxId :: TxId } |
    Local
    deriving (Eq,Show)
$(makeLenses ''ExecutionMode)


data DBVar = PureVar (MVar PureState) | PSLVar (MVar PSL)

data CommandEnv = CommandEnv {
      _ceConfig :: CommandConfig
    , _ceMode :: ExecutionMode
    , _ceDBVar :: DBVar
    , _ceState :: MVar CommandState
    }
$(makeLenses ''CommandEnv)

data CommandException = CommandException String deriving (Typeable)
instance Show CommandException where show (CommandException e) = e
instance Exception CommandException


type CommandM a = ReaderT CommandEnv IO a

runCommand :: CommandEnv -> CommandM a -> IO a
runCommand e a = runReaderT a e

throwCmdEx :: MonadThrow m => String -> m a
throwCmdEx = throw . CommandException


newtype InboundPactChan = InboundPactChan (Chan [Command ByteString])
newtype OutboundPactChan = OutboundPactChan (TChan [CommandResult])

initChans :: IO (InboundPactChan,OutboundPactChan)
initChans = (,) <$> (InboundPactChan <$> newChan) <*> (OutboundPactChan <$> atomically newTChan)

writeInbound :: InboundPactChan -> [Command ByteString] -> IO ()
writeInbound (InboundPactChan c) = writeChan c

readInbound :: InboundPactChan -> IO [Command ByteString]
readInbound (InboundPactChan c) = readChan c

writeOutbound :: OutboundPactChan -> [CommandResult] -> IO ()
writeOutbound (OutboundPactChan c) = atomically . writeTChan c

tryReadOutbound :: OutboundPactChan -> IO (Maybe [CommandResult])
tryReadOutbound (OutboundPactChan c) = atomically $ tryReadTChan c