{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Persistence and back-end DB types.
--
module Pact.Types.Persistence
  (
   Persistable(..),ToPersistable(..),
   ColumnId(..),
   RowKey(..),
   Columns(..),columns,
   Domain(..),
   TxLog(..),txDomain,txKey,txValue,
   WriteType(..),
   Method,
   PactDb(..),
   TxId(..)
   ) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Decimal (Decimal,DecimalRaw(..))
import Data.Default (Default)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.String (IsString(..))
import Data.Thyme.Time.Core (Day(..),UTCTime,UTCView(..),fromMicroseconds,mkUTCTime,unUTCTime,toMicroseconds)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Pact.Types.Lang
import Pact.Types.Util (AsString(..))

-- | Min, max values that Javascript doesn't mess up.
--
--   http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html
--   "The integer part of the Number type in Javascript is safe in [-253 .. 253] (253 = 9 007 199 254 740 992).
--    Beyond this there will be precision loss on the least significant numbers."
jsIntegerBounds :: (Integer, Integer)
jsIntegerBounds = (-9007199254740991,9007199254740991)

-- | JSON codec pair.
data Codec a = Codec {
  encoder :: a -> Value,
  decoder :: Value -> Parser a
  }

integerCodec :: Codec Integer
integerCodec = Codec encodeInteger decodeInteger
  where
    encodeInteger i = let (l,h) = jsIntegerBounds in
                        if i >= l && i <= h then Number (fromIntegral i)
                        else object [ field .= show i ]
    {-# INLINE encodeInteger #-}
    decodeInteger (Number i) = return (round i)
    decodeInteger (Object o) = do
      s <- o .: field
      case readMaybe (unpack s) of
        Just i -> return i
        Nothing -> fail $ "Invalid integer value: " ++ show o
    decodeInteger v = fail $ "Invalid integer value: " ++ show v
    {-# INLINE decodeInteger #-}
    field = "_P_int"

decimalCodec :: Codec Decimal
decimalCodec = Codec enc dec
  where
    enc (Decimal dp dm) =
      object [ places .= dp,
               mantissa .= encoder integerCodec dm ]
    {-# INLINE enc #-}
    dec = withObject "Decimal" $ \o ->
      Decimal <$> o .: places <*>
      (o .: mantissa >>= decoder integerCodec)
    {-# INLINE dec #-}
    places = "_P_decp"
    mantissa = "_P_decm"

timeCodec :: Codec UTCTime
timeCodec = Codec enc dec
  where
    enc t = object [ day .= d,
                     micros .= encoder integerCodec (fromIntegral (toMicroseconds s)) ]
      where (UTCTime (ModifiedJulianDay d) s) = unUTCTime t
    {-# INLINE enc #-}
    dec = withObject "UTCTime" $ \o ->
      mkUTCTime <$> (ModifiedJulianDay <$> o .: day) <*>
      (fromMicroseconds . fromIntegral <$> (o .: micros >>= decoder integerCodec))
    {-# INLINE dec #-}
    day = "_P_timed"
    micros = "_P_timems"

valueCodec :: Codec Value
valueCodec = Codec enc dec
  where
    enc v = object [field .= v]
    {-# INLINE enc #-}
    dec = withObject "Value" $ \o -> o .: field
    {-# INLINE dec #-}
    field = "_P_val"

keysetCodec :: Codec KeySet
keysetCodec = Codec enc dec
  where
    enc (KeySet ks p) = object [ keyf .= ks, predf .= p ]
    {-# INLINE enc #-}
    dec  = withObject "KeySet" $ \o -> KeySet <$> o .: keyf <*> o .: predf
    {-# INLINE dec #-}
    keyf = "_P_keys"
    predf = "_P_pred"


-- | Represent Pact 'Term' values that can be stored in a database.
data Persistable =
    PLiteral Literal |
    PKeySet KeySet |
    PValue Value
    deriving (Eq,Generic)
instance Show Persistable where
    show (PLiteral l) = show l
    show (PKeySet k) = show k
    show (PValue v) = BSL.toString $ encode v
instance ToTerm Persistable where
    toTerm (PLiteral l) = toTerm l
    toTerm (PKeySet k) = toTerm k
    toTerm (PValue v) = toTerm v
instance ToJSON Persistable where
    toJSON (PLiteral (LString s)) = String s
    toJSON (PLiteral (LBool b)) = Bool b
    toJSON (PLiteral (LInteger n)) = encoder integerCodec n
    toJSON (PLiteral (LDecimal d)) = encoder decimalCodec d
    toJSON (PLiteral (LTime t)) = encoder timeCodec t
    toJSON (PKeySet k) = encoder keysetCodec k
    toJSON (PValue v) = encoder valueCodec v
instance FromJSON Persistable where
    parseJSON (String s) = return (PLiteral (LString s))
    parseJSON (Number n) = return (PLiteral (LInteger (round n)))
    parseJSON (Bool b) = return (PLiteral (LBool b))
    parseJSON v@Object {} = (PLiteral . LInteger <$> decoder integerCodec v) <|>
                            (PLiteral . LDecimal <$> decoder decimalCodec v) <|>
                            (PLiteral . LTime <$> decoder timeCodec v) <|>
                            (PValue <$> decoder valueCodec v) <|>
                            (PKeySet <$> decoder keysetCodec v)
    parseJSON Null = return (PValue Null)
    parseJSON va@Array {} = return (PValue va)

class ToPersistable t where
  toPersistable :: t -> Persistable
instance ToPersistable Literal where toPersistable = PLiteral
instance ToPersistable KeySet where toPersistable = PKeySet
instance ToPersistable Value where toPersistable = PValue
instance Show n => ToPersistable (Term n) where
  toPersistable (TLiteral v _) = toPersistable v
  toPersistable (TKeySet ks _) = toPersistable ks
  toPersistable (TValue v _) = toPersistable v
  toPersistable t = toPersistable (toJSON t)

-- | Row key type for user tables.
newtype RowKey = RowKey Text
    deriving (Eq,Ord,IsString,ToTerm,AsString)
instance Show RowKey where show (RowKey s) = show s

-- | Column key type.
newtype ColumnId = ColumnId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show ColumnId where show (ColumnId s) = show s

-- | User table row-value type, mapping column ids to values.
newtype Columns v = Columns { _columns :: M.Map ColumnId v }
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (ToJSON v) => ToJSON (Columns v) where
    toJSON (Columns m) = object . map (\(k,v) -> asString k .= toJSON v) . M.toList $ m
    {-# INLINE toJSON #-}
instance (FromJSON v) => FromJSON (Columns v) where
    parseJSON = withObject "Columns" $ \o ->
                (Columns . M.fromList) <$>
                 forM (HM.toList o)
                  (\(k,v) -> ((,) <$> pure (ColumnId k) <*> parseJSON v))
    {-# INLINE parseJSON #-}

makeLenses ''Columns

-- | Specify key and value types for database domains.
data Domain k v where
  UserTables :: !TableName -> Domain RowKey (Columns Persistable)
  KeySets :: Domain KeySetName KeySet
  Modules :: Domain ModuleName Module
deriving instance Eq (Domain k v)
deriving instance Show (Domain k v)
instance AsString (Domain k v) where
    asString (UserTables t) = asString t
    asString KeySets = "SYS:KeySets"
    asString Modules = "SYS:Modules"

-- | Transaction record.
-- Backends are expected to return "user-visible" values
-- for '_txValue', namely that internal JSON formats for 'Persistable'
-- need to be converted to Term JSON formats.
data TxLog v =
    TxLog {
      _txDomain :: !Text
    , _txKey :: !Text
    , _txValue :: !v
    } deriving (Eq,Show,Typeable,Generic,Foldable,Functor,Traversable)
makeLenses ''TxLog
instance Hashable v => Hashable (TxLog v)

instance ToJSON v => ToJSON (TxLog v) where
    toJSON (TxLog d k v) =
        object ["table" .= d, "key" .= k, "value" .= v]
instance FromJSON v => FromJSON (TxLog v) where
    parseJSON = withObject "TxLog" $ \o ->
                TxLog <$> o .: "table" <*> o .: "key" <*> o .: "value"

-- | Instruction for '_writeRow'.
data WriteType =
  -- | Insert a new row, fail if key already found.
  --   Requires complete row value, enforced by pact runtime.
  Insert |
  -- | Update an existing row, fail if key not found.
  --   Allows incomplete row values.
  Update |
  -- | Update an existing row, or insert a new row if not found.
  --   Requires complete row value, enforced by pact runtime.
  Write
  deriving (Eq,Show)



-- | Transaction ids are non-negative 64-bit integers and
--   are expected to be monotonically increasing.
newtype TxId = TxId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON,Generic)

instance NFData TxId
instance Show TxId where show (TxId s) = show s
instance ToTerm TxId where toTerm = tLit . LInteger . fromIntegral
instance AsString TxId where asString = pack . show




-- | Shape of back-end methods: use MVar for state, run in IO.
type Method e a = MVar e -> IO a

-- | Fun-record type for Pact back-ends.
data PactDb e = PactDb {
    -- | Read a domain value at key, throwing an exception if not found.
    _readRow :: forall k v . (IsString k,FromJSON v) =>
                Domain k v -> k -> Method e (Maybe v)
    -- | Write a domain value at key. WriteType argument governs key behavior.
  , _writeRow :: forall k v . (AsString k,ToJSON v) =>
                 WriteType -> Domain k v -> k -> v -> Method e ()
    -- | Retrieve all keys for user table.
  , _keys ::  TableName -> Method e [RowKey]
    -- | Retrieve all transaction ids greater than supplied txid for table.
  , _txids ::  TableName -> TxId -> Method e [TxId]
    -- | Create a user table.
  , _createUserTable ::  TableName -> ModuleName -> KeySetName -> Method e ()
    -- | Get module, keyset for user table.
  , _getUserTableInfo ::  TableName -> Method e (ModuleName,KeySetName)
    -- | Initiate transaction. If TxId not provided, commit fails/rolls back.
  , _beginTx :: Maybe TxId -> Method e ()
    -- | Commit transaction, if in tx. If not in tx, rollback and throw error.
    -- Return raw TxLogs, for use in checkpointing only (not for transmission to user).
  , _commitTx ::  Method e [TxLog Value]
    -- | Rollback database transaction.
  , _rollbackTx :: Method e ()
    -- | Get transaction log for table. TxLogs are expected to be user-visible format.
  , _getTxLog :: forall k v . (IsString k,FromJSON v) =>
                 Domain k v -> TxId -> Method e [TxLog v]
}
