import Bio.Sam.Sam
import Bio.Sam.SamParser

import Conduit
import Data.Conduit.Attoparsec

main :: IO ()
main = do
  alns <- runConduitRes $
          sourceFileBS "test/data/aln.sam" .|  
--          sourceFileBS "test/data/long_cigar.sam" .|
          sinkParser samParser
  print alns
