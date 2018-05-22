import Bio.Sam.Sam
import Bio.Sam.IO

main :: IO ()
main = do
  sam <- readSamFile "test/data/aln.sam"
  print sam
