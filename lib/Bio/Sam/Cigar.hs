{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Control.Lens

--             'M'     'I'   'D'   'N'    'S'        'H'        'P'       '='     'X'
data CigarOp = Match | Ins | Del | Skip | SoftClip | HardClip | Padding | Equal | NotEqual deriving (Eq, Show)

data Cigar = Cigar {
  _cigarLen :: !Word,
  _cigarOp  :: !CigarOp
  } deriving (Eq, Show)

makeLenses ''Cigar
