{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam
where

import Data.ByteString.Char8
import Bio.BioSeq.DNASeq
import qualified Bio.Sam.Cigar as CIG
import Bio.Sam.Header
import Control.Lens
import Data.Default
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics

data AlnOptValue =
  AlnOptChar        Char               |
  AlnOptInt         Int                |
  AlnOptDouble      Double             |
  AlnOptString      T.Text             |
  AlnOptByteArray   ByteString         |
  AlnOptIntArray    (UV.Vector Int)    |
  AlnOptDoubleArray (UV.Vector Double)
  deriving (Generic, Show)

data AlnOpt = AlnOpt {
  _alnOptTag   :: !T.Text,
  _alnOptValue :: !AlnOptValue
  } deriving (Generic, Show)

makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !T.Text,
  _flag   :: !Word,
  _rname  :: !(Maybe T.Text),
  _pos    :: !(Maybe Int), -- ^ 0-origin
  _mapq   :: !(Maybe Int),
  _cigars :: !(Maybe (UV.Vector CIG.Cigar)),
  _rnext  :: !(Maybe T.Text),
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
