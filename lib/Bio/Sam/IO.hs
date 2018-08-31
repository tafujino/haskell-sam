module Bio.Sam.IO
  (readRawSamFile
  )
where

import Conduit
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString.Char8
import Bio.Sam.RawSam
import Bio.Sam.Parse

readRawSamFile :: FilePath -> IO RawSam
readRawSamFile path = runConduitRes $ sourceFileBS path .| sinkParser rawSamParser
