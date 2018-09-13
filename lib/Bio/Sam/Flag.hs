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

import Bio.Sam
import Control.Lens
import Data.Bits

testFlag :: Int -> Aln -> Bool
testFlag n aln = testBit (aln ^. flag) n

hasMultipleSegments :: Aln -> Bool
hasMultipleSegments = testFlag 0

isProperlyAligned :: Aln -> Bool
isProperlyAligned = testFlag 1

isUnmapped :: Aln -> Bool
isUnmapped = testFlag 2

isNextUnmapped :: Aln -> Bool
isNextUnmapped = testFlag 3

isReverseComplement :: Aln -> Bool
isReverseComplement = testFlag 4

isNextReverseComplement :: Aln -> Bool
isNextReverseComplement = testFlag 5

isFirstSegment :: Aln -> Bool
isFirstSegment = testFlag 6

isLastSegment :: Aln -> Bool
isLastSegment = testFlag 7

isSecondary :: Aln -> Bool
isSecondary = testFlag 8

hasNotPassedFilters :: Aln -> Bool
hasNotPassedFilters = testFlag 9

isDuplicate :: Aln -> Bool
isDuplicate = testFlag 10

isSupplementary :: Aln -> Bool
isSupplementary = testFlag 11
