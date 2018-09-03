import Bio.Sam.RawSam
import Bio.Sam.IO

main :: IO ()
main = do
  sam <- readSamFile "test/data/aln.sam"
  print sam
  -- sam' <- readSamFile "test/data/long_cigar.sam"
  -- print sam'
