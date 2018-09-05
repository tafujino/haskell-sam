{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Cigar
where

import Control.Lens
import Data.Vector.Unboxed.Deriving
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
  } deriving (Eq, Generic)

makeLenses ''Cigar

derivingUnbox "Cigar"
  [t| Cigar -> (CigarLen, CigarOp) |]
  [e| \(Cigar len op) -> (len, op) |]
  [e| uncurry Cigar                |]

instance Show Cigar where
  show (Cigar len op) = show len ++ [toChar op]

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

toChar :: CigarOp -> Char
toChar op
  | op == match    = 'M'
  | op == ins      = 'I'
  | op == del      = 'D'
  | op == skip     = 'N'
  | op == softClip = 'S'
  | op == hardClip = 'H'
  | op == padding  = 'P'
  | op == equal    = '='
  | op == notEqual = 'X'
  | otherwise       = error $ "invalid CIGAR: " ++ show op
