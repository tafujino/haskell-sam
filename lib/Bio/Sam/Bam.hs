{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (take)
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Word
import Data.Char
import Data.Bits
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (take, count, map)
--import qualified Data.ByteString.Char8 as B8
--import Data.ByteString.Base16
import Conduit
import Data.Conduit.Zlib
import Data.Attoparsec.ByteString
import Data.Conduit.Attoparsec
import Data.Attoparsec.Binary
import Data.Binary.IEEE754
--import GHC.Float

data Cigar = Cigar {
  cigLen :: Word,
  cigOp  :: Char
  } deriving Show

data AlnOptValue =
  AlnOptChar Char            |
  AnlOptInt8 Int8            |
  AlnOptUInt8 Word8          |
  AnlOptInt16 Int16          |
  AlnOptUInt16 Word16        | 
  AnlOptInt32 Int32          |
  AlnOptUInt32 Word32        | 
  AlnOptFloat Float          |
  AlnOptString String        |
  AlnOptByteArray ByteString |
  AlnOptInt8Array [Int8]     |
  AlnOptUInt8Array [Word8]   |
  AlnOptInt16Array [Int16]   |
  AlnOptUInt16Array [Word16] |
  AlnOptInt32Array [Int32]   |
  AlnOptUInt32Array [Word32] |
  AlnOptFloatArray  [Float]
  deriving Show

data AlnOpt = AlnOpt {
  alnOptTag   :: String,
  alnOptValue :: AlnOptValue
  } deriving Show

data Aln = Aln {
  qname  :: !String,
  flag   :: !Word16,
  rname  :: !(Maybe String),
  pos    :: !(Maybe Word32),
  mapq   :: !(Maybe Word8),
  cigars :: !(Maybe [Cigar]),
  rnext  :: !(Maybe String),
  pnext  :: !(Maybe Word32),
  tlen   :: !Int32,
  seq    :: !(Maybe String),
  qual   :: !(Maybe String),
  opt    :: ![AlnOpt]
  } deriving Show

int32  = fromIntegral <$> anyWord32le :: Parser Int32
uint32 = fromIntegral <$> anyWord32le :: Parser Word32

count' :: Integral a => a -> Parser b -> Parser [b]
count' = count . fromIntegral

main :: IO ()
main = do
  list <- runConduitRes $
       stdinC .|
       peekForever bamParser .| -- should check eof in the future revision
       conduitParser bamFieldsP .|
       mapC snd .|
       sinkList
  mapM_ print list
--       stdoutC  

bamParser = conduitParser bgzfBlockP .|
            mapC snd .|
            -- window bits for zlib is -15 (negative when no header is given)
            -- see https://github.com/biopython/biopython/blob/master/Bio/bgzf.py       
            decompress (WindowBits (-15)) -- .|
--            conduitParser bamFieldsP .|
--            mapC snd

-- see https://samtools.github.io/hts-specs/SAMv1.pdf
-- see https://github.com/samtools/htslib/blob/f31f2c00e8259739dac00c42ea28229c77beb6bd/bgzf.c
bgzfBlockP :: Parser ByteString
bgzfBlockP = do
  word8        31                              -- ID1:   gzip IDentifier1
  word8       139                              -- ID2:   gzip IDentifier2
  word8         8                              -- CM:    gzip Compression Method
  word8         4                              -- FLG:   gzip FLaGs
  anyWord32le                                  -- MTIME: gzip Modification TIME
  anyWord8                                     -- XFL:   gzip eXtra FLags
  anyWord8                                     -- OS:    gzip Operating System
  word16le      6                              -- XLEN:  gzip eXtra LENgth
  -- thereafter are extra subfields
  word8        66                              -- SI1:   Subfield Identifier1 = 'B'
  word8        67                              -- SI2:   Subfield Identifier2 = 'C'
  word16le      2                              -- SLEN:  Subfield LENgth
  blockSize <- anyWord16le                     -- BSIZE: total Block SIZE minus 1
  cdata <- take $ fromIntegral blockSize - 25  -- CDATA: Compressed DATA by zlib::deflate()
                                               --        # of bytes = BSIZE - XLEN(6) - 19 
  anyWord32le                                  -- CRC:   CRC-32
  anyWord32le                                  -- ISIZE: Input SIZE (length of uncompressed data)
  return cdata

bamFieldsP = do
  string "BAM\1"                                -- magic:      BAM magic string
  lText <- int32                                -- l_text:     Length of the header text
  text <- count' lText anyChar                  -- text:       Plain header text
  nRef <- int32                                 -- n_ref:      # of reference sequences
  refs <- replicateM (fromIntegral nRef) $ do
    lName <- int32                              -- l_name:     Lenght of the reference name + 1
    name <- count' lName anyChar                -- name:       Reference sequence name
    lRef <- int32                               -- l_ref:      Length of the reference sequence
    return (name, lRef)
  blockSize <- int32                            -- block_size: Length of the remainder of the alignment record
{-
  refID <- refIDP nRef                          -- refID:      Reference sequence ID
  pos <- int32                                  -- pos:        0-based leftmost coordinate
  (bin, mapq, lReadName) <- binMqNl <$> uint32  -- bin_mq_nl:  bin << 16 | MAPQ << 8 | l_read_name
  (flag, nCigarOp) <- flagNc <$> uint32         -- flag_nc:    FLAG << 16 | n_cigar_op
  lSeq <- int32                                 -- l_seq:      Length of SEQ
  nextRefID <- refIDP nRef                      -- next_refID: Ref-ID of the next segment
  nextPos <- int32                              -- next_pos:   0-based leftmost pos of the next segment
  tlen <- int32                                 -- tlen:       Template length
  readName <- count' lReadName anyChar          -- read_name:  Read name
  cigar <- count' nCigarOp cigar1P              -- CIGAR:      op_len << 4 | op
  seq <- seqP lSeq                              -- seq:        4-bit encoded read
  qual <- qualP lSeq                            -- qual:       Phred base quality
-}
--  opt <- many opt1P                             -- list of auxiliary data
  takeByteString
  return blockSize
      
refIDP :: Int32 -> Parser Int32
refIDP nRef = do
  id <- int32
--  guard $ -1 <= id && id <= nRef
  return id

binMqNl :: Word32 -> (Word16, Word8, Word8)
binMqNl n = (fromIntegral $ shiftR n 16,
             fromIntegral $ shiftR (n .&. 0xff00) 8,
             fromIntegral $ n .&. 0xff)

flagNc :: Word32 -> (Word16, Word16)
flagNc n = (fromIntegral $ shiftR n 16,
            fromIntegral $ n .&. 0xff)

cigar1P :: Parser Cigar
cigar1P = do
  x <- uint32
  let len = fromIntegral $ shiftR x 4
      opNum = x .&. 0xf
      op = case opNum of
             0 -> 'M'
             1 -> 'I'
             2 -> 'D'
             3 -> 'N'
             4 -> 'S'
             5 -> 'H'
             6 -> 'P'
             7 -> '='
             8 -> 'X'
             _ -> error $ "invalid cigar op: " ++ show opNum
  return $ Cigar len op

seqP :: Int32 -> Parser String
seqP n = map numToBaseChar <$> seqNumP n

seqNumP :: Int32 -> Parser [Word8]
seqNumP 0 = return []
seqNumP 1 = (:[]) . (`shiftR` 4) <$> anyWord8
seqNumP n = anyWord8 >>= \x -> (shiftR x 4:) . (x .&. 0xf:) <$> seqNumP (n - 2)
    
numToBaseChar :: Word8 -> Char
numToBaseChar n = case n of
                    0  -> '='
                    1  -> 'A'
                    2  -> 'C'
                    3  -> 'M'
                    4  -> 'G'
                    5  -> 'R'
                    6  -> 'S'
                    7  -> 'V'
                    8  -> 'T'
                    9  -> 'W'
                    10 -> 'Y'
                    11 -> 'H'
                    12 -> 'K'
                    13 -> 'D'
                    14 -> 'B'
                    _  -> 'N'

qualP :: Int32 -> Parser (Maybe String)
qualP n = Nothing <$  count' n (word8 0xff) <|>
          Just    <$> count' n anyChar

{--
opt1P :: Char -> Parser AlnOptValue -> Parser AlnOpt
opt1P c p = AlnOpt <$> count 2 anyChar <* char c <*> p

optCharP :: Parser AlnOpt
optCharP = opt1P 'A' $ AlnOptChar <$> anyChar

optUInt8P :: Parser AlnOpt
optUInt8P = opt1P 'c' $ AlnOptUInt8 <$> anyWord8
--}

