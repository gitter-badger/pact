{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Pact.Types.Native where

import Pact.Types.Util
import Pact.Types.Runtime
import qualified Data.Map.Strict as M
import Control.Arrow

data SpecialForm =
  WithRead |
  WithDefaultRead |
  Bind |
  Select |
  Where
  deriving (Eq,Enum,Ord,Bounded)

instance AsString SpecialForm where
  asString WithRead = "with-read"
  asString WithDefaultRead = "with-default-read"
  asString Bind = "bind"
  asString Select = "select"
  asString Where = "where"

instance Show SpecialForm where show = show . asString

specialForm :: SpecialForm -> NativeDefName
specialForm = NativeDefName . asString

sfLookup :: M.Map NativeDefName SpecialForm
sfLookup = M.fromList $ map (specialForm &&& id) [minBound .. maxBound]

isSpecialForm :: NativeDefName -> Maybe SpecialForm
isSpecialForm = (`M.lookup` sfLookup)


-- | Native function with un-reduced arguments. Must fire call stack.
type NativeFun e = FunApp -> [Term Ref] -> Eval e (Gas,Term Name)

-- | Native function with pre-reduced arguments, call stack fired.
type RNativeFun e = FunApp -> [Term Name] -> Eval e (Gas,Term Name)


type NativeDef = (NativeDefName,Term Name)
type NativeModule = (ModuleName,[NativeDef])

data ReadValue =
  ReadRow (Columns Persistable) |
  ReadKey RowKey |
  ReadTxId TxId |
  ReadTxLog (TxLog (Columns Persistable)) |
  ReadKeyLog (TxId, TxLog (Columns Persistable))

class Readable a where
  readable :: a -> ReadValue

instance Readable (Columns Persistable) where
  readable = ReadRow
instance Readable RowKey where
  readable = ReadKey
instance Readable TxId where
  readable = ReadTxId
instance Readable (TxLog (Columns Persistable)) where
  readable = ReadTxLog
instance Readable (TxId, TxLog (Columns Persistable)) where
  readable = ReadKeyLog

data GasSpecial =
  GPostRead ReadValue |
  GSelect (Maybe [(Info,ColumnId)]) (Term Ref) (Term Name)
