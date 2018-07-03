{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Control.DeepSeq
import Control.Lens
import GHC.Generics

--             'M'     'I'   'D'   'N'    'S'        'H'        'P'       '='     'X'
data CigarOp = Match | Ins | Del | Skip | SoftClip | HardClip | Padding | Equal | NotEqual deriving (Eq, Generic, Show)

instance NFData CigarOp

type CigarLen = Word

data Cigar = Cigar {
  _cigarLen :: !CigarLen,
  _cigarOp  :: !CigarOp
  } deriving (Eq, Generic, Show)

instance NFData Cigar
makeLenses ''Cigar
