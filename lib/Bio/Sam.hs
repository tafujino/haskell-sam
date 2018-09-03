{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam
where

import Data.ByteString.Char8
import Bio.NASeq.DNASeq
import Bio.Sam.Cigar
import Bio.Sam.Header
import Control.Lens
import Data.Default
import Data.Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics

data AlnOptValue =
  AlnOptChar        Char               |
  AlnOptInt         Int                |
  AlnOptDouble      Double             |
  AlnOptString      Text               |
  AlnOptByteArray   ByteString         |
  AlnOptIntArray    (UV.Vector Int)    |
  AlnOptDoubleArray (UV.Vector Double)
  deriving (Generic, Show)

data AlnOpt = AlnOpt {
  _alnOptTag   :: !ByteString,
  _alnOptValue :: !AlnOptValue
  } deriving (Generic, Show)

makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !ByteString,
  _flag   :: !Word,
  _rname  :: !(Maybe ByteString),
  _pos    :: !(Maybe Int), -- ^ 0-origin
  _mapq   :: !(Maybe Int),
  _cigars :: !(Maybe (V.Vector Cigar)),
  _rnext  :: !(Maybe ByteString),
  _pnext  :: !(Maybe Int),
  _tlen   :: !Int,
  _seq    :: !(Maybe DNASeq),
  _qual   :: !(Maybe ByteString),
  _opt    :: !(V.Vector AlnOpt)
  } deriving (Generic, Show)

makeLenses ''Aln

data Sam = Sam {
  _header :: Header,
  _alns   :: V.Vector Aln
  } deriving (Generic, Show)

makeLenses ''Sam
