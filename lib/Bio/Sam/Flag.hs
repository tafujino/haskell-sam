module Bio.Sam.Flag
  (hasMultipleSegments,
   isProperlyAligned,
   isUnmapped,
   isNextUnmapped,
   isReverseComplement,
   isNextReverseComplement,
   isFirstSegment,
   isLastSegment,
   isSecondary,
   hasNotPassedFilters,
   isDuplicate,
   isSupplementary
  )
where

import Bio.Sam.Sam
import Control.Lens
import Data.Bits

testFlag :: Int -> Aln -> Bool
testFlag n aln = testBit (aln ^. flag) n

hasMultipleSegments :: Aln -> Bool
hasMultipleSegments = testFlag 0x1

isProperlyAligned :: Aln -> Bool
isProperlyAligned = testFlag 0x2

isUnmapped :: Aln -> Bool
isUnmapped = testFlag 0x4

isNextUnmapped :: Aln -> Bool
isNextUnmapped = testFlag 0x8

isReverseComplement :: Aln -> Bool
isReverseComplement = testFlag 0x10

isNextReverseComplement :: Aln -> Bool
isNextReverseComplement = testFlag 0x20

isFirstSegment :: Aln -> Bool
isFirstSegment = testFlag 0x40

isLastSegment :: Aln -> Bool
isLastSegment = testFlag 0x80

isSecondary :: Aln -> Bool
isSecondary = testFlag 0x100

hasNotPassedFilters :: Aln -> Bool
hasNotPassedFilters = testFlag 0x200

isDuplicate :: Aln -> Bool
isDuplicate = testFlag 0x400

isSupplementary :: Aln -> Bool
isSupplementary = testFlag 0x800
