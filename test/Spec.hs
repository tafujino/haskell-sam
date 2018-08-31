import Bio.Sam.StrictSam
import Bio.Sam.IO

main :: IO ()
main = do
  sam <- readStrictSamFile "test/data/aln.sam"
  print sam
