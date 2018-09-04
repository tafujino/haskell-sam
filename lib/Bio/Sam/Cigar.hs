{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Data.Vector.Unboxed.Deriving
import Control.Lens
import GHC.Generics

type CigarOp = Int

-- | 'M'
match    :: CigarOp
match    = 0

-- | 'I'
ins      :: CigarOp
ins      = 1

-- | 'D'
del      :: CigarOp
del      = 2

-- | 'N'
skip     :: CigarOp
skip     = 3

-- | 'S'
softClip :: CigarOp
softClip = 4

-- | 'H'
hardClip :: CigarOp
hardClip = 5

-- | 'P'
padding  :: CigarOp
padding  = 6

-- | '='
equal    :: CigarOp
equal    = 7

-- | 'X'
notEqual :: CigarOp
notEqual = 8

type CigarLen = Int

data Cigar = Cigar {
  cigarLen :: !CigarLen,
  cigarOp  :: !CigarOp
  } deriving (Eq, Generic, Show)

makeLenses ''Cigar

derivingUnbox "Cigar"
  [t| Cigar -> (CigarLen, CigarOp) |]
  [e| \(Cigar len op) -> (len, op) |]
  [e| uncurry Cigar                |]

fromChar :: Char -> CigarOp
fromChar 'M' = match
fromChar 'I' = ins
fromChar 'D' = del
fromChar 'N' = skip
fromChar 'S' = softClip
fromChar 'H' = hardClip
fromChar 'P' = padding
fromChar '=' = equal
fromChar 'X' = notEqual
fromChar c   = error $ "invalid CIGAR op: " ++ [c]

toChar cig
  | cig == match    = 'M'
  | cig == ins      = 'I'
  | cig == del      = 'D'
  | cig == skip     = 'N'
  | cig == softClip = 'S'
  | cig == hardClip = 'H'
  | cig == padding  = 'P'
  | cig == equal    = '='
  | cig == notEqual = 'X'
  | otherwise       = error $ "invalid CIGAR: " ++ show cig
