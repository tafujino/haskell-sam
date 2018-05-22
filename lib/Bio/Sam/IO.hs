module Bio.Sam.IO
  (readSamFile
  )
where

import Conduit
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString.Char8
import Bio.Sam.Sam
import Bio.Sam.Parse

readSamFile :: FilePath -> IO Sam
readSamFile path = runConduitRes $ sourceFileBS path .| sinkParser samParser
