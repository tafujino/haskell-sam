import Bio.Sam.RawSam
import Bio.Sam.IO

main :: IO ()
main = do
  sam <- readRawSamFile "test/data/aln.sam"
  print sam
