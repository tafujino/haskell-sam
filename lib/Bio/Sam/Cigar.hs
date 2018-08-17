{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Control.DeepSeq
import Control.Lens
import GHC.Generics

data CigarOp = Match    | -- ^ 'M'
               Ins      | -- ^ 'I'
               Del      | -- ^ 'D'
               Skip     | -- ^ 'N'
               SoftClip | -- ^ 'S'
               HardClip | -- ^ 'H'
               Padding  | -- ^ 'P'
               Equal    | -- ^ '='
               NotEqual   -- ^ 'X'
             deriving (Eq, Generic, Show)

instance NFData CigarOp

type CigarLen = Word

data Cigar = Cigar {
  _cigarLen :: !CigarLen,
  _cigarOp  :: !CigarOp
  } deriving (Eq, Generic, Show)

instance NFData Cigar
makeLenses ''Cigar
