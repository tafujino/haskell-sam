module Bio.Sam.IO
  (readStrictSamFile
  )
where

import Conduit
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString.Char8
import Bio.Sam.StrictSam
import Bio.Sam.Parse

readStrictSamFile :: FilePath -> IO StrictSam
readStrictSamFile path = runConduitRes $ sourceFileBS path .| sinkParser strictSamParser
