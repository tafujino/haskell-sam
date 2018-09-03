module Bio.Sam.IO
  (readRawSamFile,
   readSamFile
  )
where

import Conduit
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Bio.Sam.RawSam as R
import qualified Bio.Sam as S
import Bio.Sam.Parse

readFileByParser :: Parser a -> FilePath -> IO a
readFileByParser parser path = runConduitRes $ sourceFileBS path .| sinkParser parser

readRawSamFile :: FilePath -> IO R.Sam
readRawSamFile = readFileByParser rawSamParser

readSamFile :: FilePath -> IO S.Sam
readSamFile = readFileByParser samParser
