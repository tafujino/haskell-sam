{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.RawSam
where

import qualified Bio.Sam.Cigar as CIG
import Bio.Sam.Header
import Data.ByteString.Char8
import Data.Int
import Data.Word
import Control.Lens
import Data.Default
import Data.Sequence
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics

data AlnOptValue =
  AlnOptChar        Char              |
  AlnOptInt8        Int8              | -- only for BAM
  AlnOptUInt8       Word8             | -- only for BAM
  AlnOptInt16       Int16             | -- only for BAM
  AlnOptUInt16      Word16            | -- only for BAM
  AlnOptInt32       Int32             |
  AlnOptUInt32      Word32            | -- only for BAM
  AlnOptFloat       Float             |
  AlnOptString      ByteString        |
  AlnOptByteArray   ByteString        |
  AlnOptInt8Array   (UV.Vector Int8)   |
  AlnOptUInt8Array  (UV.Vector Word8)  |
  AlnOptInt16Array  (UV.Vector Int16)  |
  AlnOptUInt16Array (UV.Vector Word16) |
  AlnOptInt32Array  (UV.Vector Int32)  |
  AlnOptUInt32Array (UV.Vector Word32) |
  AlnOptFloatArray  (UV.Vector Float)
  deriving (Generic, Show)

data AlnOpt = AlnOpt {
  _alnOptTag   :: !T.Text,
  _alnOptValue :: !AlnOptValue
  } deriving (Generic, Show)

makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !T.Text,
  _flag   :: !Word16,
  _rname  :: !(Maybe T.Text),
  _pos    :: !(Maybe Word32), -- ^ 1-origin
  _mapq   :: !(Maybe Word8),
  _cigars :: !(Maybe (UV.Vector CIG.Cigar)),
  _rnext  :: !(Maybe T.Text),
  _pnext  :: !(Maybe Word32),
  _tlen   :: !Int32,
  _seq    :: !(Maybe ByteString),
  _qual   :: !(Maybe ByteString),
  _opt    :: !(V.Vector AlnOpt)
  } deriving (Generic, Show)

makeLenses ''Aln

data Sam = Sam {
  _header :: Header,
  _alns   :: V.Vector Aln
  } deriving (Generic, Show)

makeLenses ''Sam
