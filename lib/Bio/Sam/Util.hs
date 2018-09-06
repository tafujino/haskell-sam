module Bio.Sam.Util
  (alnRefLength,
   alnQueryLength,
   alnPairwiseLength
  )
where

import Bio.Sam
import qualified Bio.Sam.Cigar as CIG
import qualified Data.Vector.Unboxed as UV

-- | calculate alignment length, where cigars filtered by p are not counted
alnLengthExcept :: (CIG.Cigar -> Bool) -> UV.Vector CIG.Cigar -> Int
alnLengthExcept p = UV.sum . UV.map CIG.cigarLen . UV.filter p

alnRefLength :: UV.Vector CIG.Cigar -> Int
alnRefLength = alnLengthExcept $ (== CIG.ins) . CIG.cigarOp

alnQueryLength :: UV.Vector CIG.Cigar -> Int
alnQueryLength = alnLengthExcept $ (== CIG.del) . CIG.cigarOp

alnPairwiseLength :: UV.Vector CIG.Cigar -> Int
alnPairwiseLength = alnLengthExcept $ const False
