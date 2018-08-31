{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.RawSam
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

data AlnOptValue =
  AlnOptChar        Char       |
  AlnOptInt         Int8       | -- only for BAM
  AlnOptUInt8       Word8      | -- only for BAM
  AlnOptInt16       Int16      | -- only for BAM
  AlnOptUInt16      Word16     | -- only for BAM
  AlnOptInt32       Int32      |
  AlnOptUInt32      Word32     | -- only for BAM
  AlnOptFloat       Float      |
  AlnOptString      ByteString |
  AlnOptByteArray   ByteString |
  AlnOptInt8Array   [Int8]     |
  AlnOptUInt8Array  [Word8]    |
  AlnOptInt16Array  [Int16]    |
  AlnOptUInt16Array [Word16]   |
  AlnOptInt32Array  [Int32]    |
  AlnOptUInt32Array [Word32]   |
  AlnOptFloatArray  [Float]
  deriving (Generic, Show)

data AlnOpt = AlnOpt {
  _strictAlnOptTag   :: ByteString,
  _strictAlnOptValue :: AlnOptValue
  } deriving (Generic, Show)

makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !ByteString,
  _flag   :: !Word16,
  _rname  :: !(Maybe ByteString),
  _pos    :: !(Maybe Word32), -- 1-origin
  _mapq   :: !(Maybe Word8),
  _cigars :: !(Maybe [Cigar]),
  _rnext  :: !(Maybe ByteString),
  _pnext  :: !(Maybe Word32),
  _tlen   :: !Int32,
  _seq    :: !(Maybe ByteString),
  _qual   :: !(Maybe ByteString),
  _opt    :: ![AlnOpt]
  } deriving (Generic, Show)

makeLenses ''Aln

data RawSam = RawSam {
  _header :: Header,
  _alns   :: [Aln]
  } deriving (Generic, Show)

makeLenses ''RawSam

