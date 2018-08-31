{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.StrictSam
where

import Data.ByteString.Char8
import Data.Int
import Data.Word
import Bio.Sam.Cigar
import Bio.Sam.Header
import Control.Lens
import Data.Default
import Data.Sequence
import Data.Text
import GHC.Generics

data StrictAlnOptValue =
  StrictAlnOptChar        Char       |
  StrictAlnOptInt         Int8       | -- only for BAM
  StrictAlnOptUInt8       Word8      | -- only for BAM
  StrictAlnOptInt16       Int16      | -- only for BAM
  StrictAlnOptUInt16      Word16     | -- only for BAM
  StrictAlnOptInt32       Int32      |
  StrictAlnOptUInt32      Word32     | -- only for BAM
  StrictAlnOptFloat       Float      |
  StrictAlnOptString      ByteString |
  StrictAlnOptByteArray   ByteString |
  StrictAlnOptInt8Array   [Int8]     |
  StrictAlnOptUInt8Array  [Word8]    |
  StrictAlnOptInt16Array  [Int16]    |
  StrictAlnOptUInt16Array [Word16]   |
  StrictAlnOptInt32Array  [Int32]    |
  StrictAlnOptUInt32Array [Word32]   |
  StrictAlnOptFloatArray  [Float]
  deriving (Generic, Show)

data StrictAlnOpt = StrictAlnOpt {
  _alnOptTag   :: ByteString,
  _alnOptValue :: StrictAlnOptValue
  } deriving (Generic, Show)

makeLenses ''StrictAlnOpt

data StrictAln = StrictAln {
  _qname  :: !ByteString,
  _flag   :: !Word16,
  _rname  :: !(Maybe ByteString),
  _pos    :: !(Maybe Word32), -- change to Int64
  _mapq   :: !(Maybe Word8),
  _cigars :: !(Maybe [Cigar]),
  _rnext  :: !(Maybe ByteString),
  _pnext  :: !(Maybe Word32),
  _tlen   :: !Int32,
  _seq    :: !(Maybe ByteString),
  _qual   :: !(Maybe ByteString),
  _opt    :: ![StrictAlnOpt]
  } deriving (Generic, Show)

makeLenses ''StrictAln

data StrictSam = StrictSam {
  _header :: Header,
  _alns   :: [StrictAln]
  } deriving (Generic, Show)

makeLenses ''StrictSam

