{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Pact.Types.Gas where

import Pact.Types.Runtime


data ReadValue
  = ReadRow (Columns Persistable)
  | ReadKey RowKey
  | ReadTxId TxId
  | ReadTxLog (TxLog (Columns Persistable))
  | ReadKeyLog (TxId, TxLog (Columns Persistable))

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

data GasArgs
  = GPostRead ReadValue
  | GSelect (Maybe [(Info,ColumnId)]) (Term Ref) (Term Name)
  | GUnreduced [Term Ref]
  | GReduced [Term Name]
  | GUse ModuleName (Maybe Hash)
  | GModule Module
  | GModuleMember Module


-- | Compute gas for some application or evaluation.
computeGas :: Either Text FunApp -> GasArgs -> Eval e Gas
computeGas _ _ = return GFree -- TODO

-- | Pre-compute gas for some application before some action.
computeGas' :: FunApp -> GasArgs -> Eval e a -> Eval e (Gas,a)
computeGas' i gs action = computeGas (Right i) gs >>= \g -> (g,) <$> action

-- | Compute gas for some application with reduced args.
gas :: FunApp -> [Term Name] -> Eval e Gas
gas i as = computeGas (Right i) (GReduced as)

-- | Pre-compute gas for some application with reduced args before some action.
gas' :: FunApp -> [Term Name] -> Eval e a -> Eval e (Gas,a)
gas' fa args action = gas fa args >>= \g -> (g,) <$> action

-- | Pre-compute gas for some application with unreduced args before some action.
gasUnreduced :: FunApp -> [Term Ref] -> Eval e a -> Eval e (Gas,a)
gasUnreduced i as = computeGas' i (GUnreduced as)
