{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Control.Lens

--             'M'     'I'   'D'   'N'    'S'        'H'        'P'       '='     'X'
data CigarOp = Match | Ins | Del | Skip | SoftClip | HardClip | Padding | Equal | NotEqual deriving (Eq, Show)

type CigarLen = Word

data Cigar = Cigar {
  _cigarLen :: !CigarLen,
  _cigarOp  :: !CigarOp
  } deriving (Eq, Show)

makeLenses ''Cigar
