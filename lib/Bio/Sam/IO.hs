module Bio.Sam.IO
  (readRawSamFile,
   readSamFile,
   readRawSamStdin,
   readSamStdin
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

readStdinByParser :: Parser a -> IO a
readStdinByParser parser = runConduitRes $ stdinC .| sinkParser parser

readRawSamStdin :: IO R.Sam
readRawSamStdin = readStdinByParser rawSamParser

readSamStdin :: IO S.Sam
readSamStdin = readStdinByParser samParser
