{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Bio.Sam.Parse
  (samParser
  )
where

import Prelude hiding (seq, take, takeWhile)
import Bio.Sam.Sam
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((|>))
import Data.Default
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base16
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Sequence hiding (null, take)
import Data.Time.Clock
import Data.Time.ISO8601
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import Data.Attoparsec.Applicative
import Data.Attoparsec.ByteString.Char8
import GHC.Float
import qualified Bio.Sam.Cigar as CIG

--------------------------------------------------------------------------------

-- header parser

type LineNo = Int
data HeaderState = HeaderState {
  runHeader :: Header,
  runLineNo :: LineNo
  } deriving Show

updateMany :: (a -> Parser a) -> a -> Parser a
updateMany update = liftM2 mplus (update >=> updateMany update) return

updateAlt :: [a -> Parser a] -> a -> Parser a
updateAlt = foldl1 $ liftA2 (<|>)

isHex :: Char -> Bool
isHex = isDigit <||> 'A' <-> 'F' <||> 'a' <-> 'f'

isNucleotideBaseChar :: Char -> Bool
isNucleotideBaseChar = inClass' "ACMGRSVTWYHKDBN"

anyFieldChar :: Char -> Bool
anyFieldChar = (/= '\t') <&&> (/= '\r') <&&> (/= '\n')

anyFieldByteStringP :: Parser B8.ByteString
anyFieldByteStringP = takeWhile1 anyFieldChar

anyFieldTextP :: Parser T.Text
anyFieldTextP = decodeUtf8 <$> takeWhile1 anyFieldChar

rawFieldP :: Parser RawField
rawFieldP = tabP *> (RawField <$> take 2 <* char ':' <*> anyFieldByteStringP)

headerLineP :: (Default a) =>
               String ->
               (LineNo -> Bool) ->
               (a -> Bool) ->
               (a -> Header -> Header) ->
               [a -> Parser a] ->
               HeaderState ->
               Parser HeaderState
headerLineP tag lineNoCond valueCond update parsers (HeaderState header lineNo) = do
  char '@'
  string $ B8.pack tag
  unless (lineNoCond lineNo) $ error $ "@" ++ tag ++ " cannot be in line " ++ show lineNo
  value <- updateMany (updateAlt parsers) def
  unless (valueCond value) $ error $ "mandatory field is missing in a @" ++ tag ++ " header line"
  endOfLine
  return $ HeaderState (update value header) $ lineNo + 1

appendAt :: Lens' a (Seq b) -> b -> a -> a
appendAt loc x = loc %~ (|> x)

---

headerEmptyLineP :: HeaderState -> Parser HeaderState
headerEmptyLineP (HeaderState header lineNo) = skipSpace *> endOfLine $> HeaderState header (lineNo + 1)

---

headerTopLineP :: HeaderState -> Parser HeaderState
headerTopLineP = headerLineP
                 "HD"
                 (== 1)
                 (^. version . to isJust)
                 const
                 [versionP,
                  sortOrderP,
                  groupingP,
                  optHeaderFieldP]

headerTagValueP :: B8.ByteString -> Parser a -> Parser a
headerTagValueP tag = (tabP *> string tag *> char ':' *>)

headerFieldP :: Lens' a b -> (b -> Bool) -> B8.ByteString -> Parser b -> a -> Parser a
headerFieldP loc cond tag parser state = headerTagValueP tag $ do
  x <- parser
  unless (cond $ state ^. loc) $ error $ "tag " ++ B8.unpack tag ++ "appeared twice"
  return $ state & loc .~ x

headerMaybeFieldP :: Lens' a (Maybe b) -> B8.ByteString -> Parser b -> a -> Parser a
headerMaybeFieldP loc tag parser = headerFieldP loc isNothing tag $ Just <$> parser

headerListFieldP :: Lens' a [b] -> B8.ByteString -> Parser [b] -> a -> Parser a
headerListFieldP loc = headerFieldP loc null

headerByteStringFieldP :: Lens' a B8.ByteString -> B8.ByteString -> Parser B8.ByteString -> a -> Parser a
headerByteStringFieldP loc = headerFieldP loc B8.null

headerRawFieldP :: Lens' a (Seq RawField) -> a -> Parser a
headerRawFieldP loc state = do
  x <- rawFieldP
  return $ state & loc %~ (|> x)

enumP :: [(String, a)] -> Parser a
enumP = foldl1 (<|>) . map (\(str, val) -> string (B8.pack str) $> val)

versionP :: Header -> Parser Header
versionP = headerMaybeFieldP version "VN" $ do
  xs  <- digitString
  dot <- char '.'
  ys  <- digitString
  return $ xs `B8.append` (dot `B8.cons` ys)
  where digitString = takeWhile1 isDigit

sortOrderP :: Header -> Parser Header
sortOrderP = headerMaybeFieldP sortOrder "SO" $ enumP [("unknown",    UnknownOrder   ),
                                                       ("unsorted",   UnsortedOrder  ),
                                                       ("queryname",  QueryNameOrder ),
                                                       ("coordinate", CoordinateOrder)]

groupingP :: Header -> Parser Header
groupingP = headerMaybeFieldP grouping "GO" $ enumP [("none",      NoGroup       ),
                                                     ("query",     QueryGroup    ),
                                                     ("reference", ReferenceGroup)]

optHeaderFieldP :: Header -> Parser Header
optHeaderFieldP = headerRawFieldP optHeaderFields

-----

headerReferenceLineP :: HeaderState -> Parser HeaderState
headerReferenceLineP = headerLineP
                       "SQ"
                       (const True)
                       ((^. refName . to (not . B8.null)) <&&> (^. refLen . to (/= 0)))
                       (appendAt references)
                       [refSeqNameP,
                        refLenP,
                        refAltLocusP,
                        refAltRefNamesP,
                        refAssemblyIDP,
                        refMD5P,
                        refSpeciesP,
                        refURIP,
                        refOptFieldP
                       ]

refSeqNameP :: Reference -> Parser Reference
refSeqNameP = headerByteStringFieldP refName "SN" $
              liftA2 B8.cons (satisfy ('!' <-> ')' <||> '+' <-> '<' <||> '>' <-> '~')) (takeWhile ('!' <-> '~'))

refLenP :: Reference -> Parser Reference
refLenP = headerFieldP refLen (== 0) "LN" decimal

refAltLocusP :: Reference -> Parser Reference
refAltLocusP = headerMaybeFieldP altLocus "AH" $ starOr (Just <$> anyFieldByteStringP)

refAltRefNamesP :: Reference -> Parser Reference
refAltRefNamesP = headerListFieldP altRefNames "AN" $ (`sepBy1` char ',') $
                  liftA2 B8.cons
                  (satisfy   (isDigit <||> isAlpha_ascii))
                  (takeWhile (isDigit <||> isAlpha_ascii <||> inClass' "*+.@_|-"))

refAssemblyIDP :: Reference -> Parser Reference
refAssemblyIDP = headerMaybeFieldP assemblyID "AS" anyFieldByteStringP

refMD5P :: Reference -> Parser Reference
refMD5P = headerMaybeFieldP md5 "M5" $ takeWhile isHex

refSpeciesP :: Reference -> Parser Reference
refSpeciesP = headerMaybeFieldP species "SP" anyFieldByteStringP

refURIP :: Reference -> Parser Reference
refURIP = headerMaybeFieldP uri "UR" anyFieldByteStringP -- for now, accepts any non-empty bytestring

refOptFieldP :: Reference -> Parser Reference
refOptFieldP = headerRawFieldP refOptFields

-----

headerReadGroupLineP :: HeaderState -> Parser HeaderState
headerReadGroupLineP = headerLineP
                       "RG"
                       (const True)
                       (^. readGroupID . to (not . B8.null))
                       (appendAt readGroups)
                       [readGroupIDP,
                        readGroupSeqCenterP,
                        readGroupDescP,
                        readGroupDateP,
                        readGroupFlowOrderP,
                        readGroupKeySeqP,
                        readGroupLibraryP,
                        readGroupProgramP,
                        readGroupMedianInsertP,
                        readGroupPlatformP,
                        readGroupPlatformModelP,
                        readGroupPlatformUnitP,
                        readGroupSampleP,
                        readGroupOptFieldP
                       ]

readGroupIDP :: ReadGroup -> Parser ReadGroup
readGroupIDP = headerByteStringFieldP readGroupID "ID" anyFieldByteStringP

readGroupSeqCenterP :: ReadGroup -> Parser ReadGroup
readGroupSeqCenterP = headerMaybeFieldP sequencingCenter "CN" anyFieldByteStringP

readGroupDescP :: ReadGroup -> Parser ReadGroup
readGroupDescP = headerMaybeFieldP readGroupDesc "DS" anyFieldTextP

readGroupDateP :: ReadGroup -> Parser ReadGroup
readGroupDateP = headerMaybeFieldP date "DT" $ maybe mzero return =<< parseISO8601 . B8.unpack <$> anyFieldByteStringP

readGroupFlowOrderP :: ReadGroup -> Parser ReadGroup
readGroupFlowOrderP = headerMaybeFieldP flowOrder "FO" $
                      starOr $ Just . B8.pack <$> many1 (satisfy isNucleotideBaseChar)

readGroupKeySeqP :: ReadGroup -> Parser ReadGroup
readGroupKeySeqP = headerMaybeFieldP keySequence "KS" $ B8.pack <$> many1 (satisfy isNucleotideBaseChar)

readGroupLibraryP :: ReadGroup -> Parser ReadGroup
readGroupLibraryP = headerMaybeFieldP keySequence "LB" anyFieldByteStringP

readGroupProgramP :: ReadGroup -> Parser ReadGroup
readGroupProgramP = headerMaybeFieldP program "PG" anyFieldByteStringP

readGroupMedianInsertP :: ReadGroup -> Parser ReadGroup
readGroupMedianInsertP = headerMaybeFieldP medianInsert "PI" decimal

readGroupPlatformP :: ReadGroup -> Parser ReadGroup
readGroupPlatformP = headerMaybeFieldP platform "PL" $ enumP [("CAPILLARY",  Capillary ),
                                                              ("LS454",      LS454     ),
                                                              ("ILLUMINA",   Illumina  ),
                                                              ("SOLID",      SOLiD     ),
                                                              ("HELICOS",    Helicos   ),
                                                              ("IONTORRENT", IonTorrent),
                                                              ("ONT",        ONT       ),
                                                              ("PACBIO",     PacBio    )
                                                             ]

readGroupPlatformModelP :: ReadGroup -> Parser ReadGroup
readGroupPlatformModelP = headerMaybeFieldP platformModel "PM" anyFieldByteStringP

readGroupPlatformUnitP :: ReadGroup -> Parser ReadGroup
readGroupPlatformUnitP = headerMaybeFieldP platformUnit "PU" anyFieldByteStringP

readGroupSampleP :: ReadGroup -> Parser ReadGroup
readGroupSampleP = headerMaybeFieldP sample "SM" anyFieldByteStringP

readGroupOptFieldP :: ReadGroup -> Parser ReadGroup
readGroupOptFieldP = headerRawFieldP readGroupOptFields

-----

headerProgramLineP :: HeaderState -> Parser HeaderState
headerProgramLineP = headerLineP
                     "PG"
                     (const True)
                     (^. programID . to (not . B8.null))
                     (appendAt programs)
                     [programIDP,
                      programNameP,
                      programCmdLineP,
                      programPrevIDP,
                      programDescP,
                      programVersionP,
                      programOptFieldP
                     ]

programIDP :: Program -> Parser Program
programIDP = headerByteStringFieldP programID "ID" anyFieldByteStringP

programNameP :: Program -> Parser Program
programNameP = headerMaybeFieldP programName "PN" anyFieldByteStringP

programCmdLineP :: Program -> Parser Program
programCmdLineP = headerMaybeFieldP commandLine "CL" anyFieldTextP

programPrevIDP :: Program -> Parser Program
programPrevIDP = headerMaybeFieldP prevProgramID "PP" anyFieldByteStringP

programDescP :: Program -> Parser Program
programDescP = headerMaybeFieldP programDesc "DS" anyFieldTextP

programVersionP :: Program -> Parser Program
programVersionP = headerMaybeFieldP programVersion "VN" anyFieldByteStringP

programOptFieldP :: Program -> Parser Program
programOptFieldP = headerRawFieldP programOptFields

-----

headerCommentLineP :: HeaderState -> Parser HeaderState
headerCommentLineP = headerLineP
                     "CO"
                     (const True)
                     (const True)
                     (flip const)
                     [(tabP *> takeTill isEndOfLine' $>) :: Header -> Parser Header]

-----

-- should check uniqueness of read group ID
-- should check if @PG-PP refers to any @PG-ID
headerParser :: Parser Header
headerParser = runHeader <$>
               updateMany
               (updateAlt [headerTopLineP,
                           headerReferenceLineP,
                           headerReadGroupLineP,
                           headerProgramLineP,
                           headerCommentLineP,
                           headerEmptyLineP])
               (HeaderState def 1)

-- alignment parser

alnParser :: Parser Aln
alnParser = Aln <$>
            qnameP  <* tabP <*>
            flagP   <* tabP <*>
            rnameP  <* tabP <*>
            posP    <* tabP <*>
            mapqP   <* tabP <*>
            cigarsP <* tabP <*>
            rnextP  <* tabP <*>
            pnextP  <* tabP <*>
            tlenP   <* tabP <*>
            seqP    <* tabP <*>
            qualP           <*>
            optP

starOr :: Parser (Maybe a) -> Parser (Maybe a)
starOr p = Nothing <$ "*"
           <|> p

validIf :: (a -> Bool) -> a -> Maybe a
validIf f x = guard (f x) >> return x

tabP :: Parser Char
tabP = char '\t'

qnameP :: Parser B8.ByteString
qnameP = takeWhile ('!' <-> '?' <||> 'A' <-> '~')

flagP :: Parser Word16
flagP = decimal

rnameP :: Parser (Maybe B8.ByteString)
rnameP = starOr $ Just <$> liftA2 B8.cons (satisfy ('!' <-> '<' <||> '>' <-> '~')) (takeWhile ('!' <-> '~'))

posP :: Parser (Maybe Word32)
posP = starOr $ validIf (/= 0) <$> decimal

mapqP :: Parser (Maybe Word8)
mapqP = starOr $ Just <$> decimal

cigar1P :: Parser CIG.Cigar
cigar1P = CIG.Cigar <$> decimal <*> (charToCigar <$> satisfy (inClass' "MIDNSHP=X"))
  where
    charToCigar chr = case chr of
      'M' -> CIG.Match
      'I' -> CIG.Ins
      'D' -> CIG.Del
      'N' -> CIG.Skip
      'S' -> CIG.SoftClip
      'H' -> CIG.HardClip
      'P' -> CIG.Padding
      '=' -> CIG.Equal
      'X' -> CIG.NotEqual
      _   -> error "invalid CIG.Cigar op char"

cigarsP :: Parser (Maybe [CIG.Cigar])
cigarsP = starOr $ Just <$> many cigar1P

rnextP :: Parser (Maybe B8.ByteString)
rnextP = Just <$> "=" <|> rnameP

pnextP :: Parser (Maybe Word32)
pnextP = starOr $ validIf (/= 0) <$> decimal

tlenP :: Parser Int32
tlenP = signed decimal

seqP :: Parser (Maybe B8.ByteString)
seqP = starOr $ Just <$> takeWhile1 (isAlpha_ascii <||> (== '=') <||> (== '.'))

qualP :: Parser (Maybe B8.ByteString)
qualP = starOr $ Just <$> takeWhile1 ('!' <-> '~')

opt1P :: Char -> Parser AlnOptValue -> Parser AlnOpt
opt1P c p = AlnOpt <$> tagP <* char ':' <* char c <* char ':' <*> p
  where tagP = B8.pack <$> satisfy isAlpha_ascii <:> satisfy (isAlpha_ascii <||> isDigit) <:> pure []

optCharP :: Parser AlnOpt
optCharP = opt1P 'A' $ AlnOptChar <$> anyChar

optIntP :: Parser AlnOpt
optIntP = opt1P 'i' $ AlnOptInt32 <$> signed decimal

optFloatP :: Parser AlnOpt
optFloatP = opt1P 'f' $ AlnOptFloat . double2Float <$> signed double

optStringP :: Parser AlnOpt
optStringP = opt1P 'Z' $ AlnOptString <$> takeWhile (' ' <-> '~')

optByteArrayP :: Parser AlnOpt
optByteArrayP = opt1P 'H' $ do
  (result, invalid) <- decode <$> takeWhile isHex
  -- function 'hexadecimal' outputs a variable of class Integral, but in this case
  -- outputting ByteString may be more preferable
  -- when invalid characters exist or the length is odd, this gives mzero (without error messages)
  -- see https://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Internal-Types.html#t:Parser
  guard $ B8.null invalid
  return $ AlnOptByteArray result

optArrayP :: Parser AlnOpt
optArrayP = opt1P 'B' $
  foldl1 (<|>) [valueP 'c' AlnOptInt8Array  $ signed decimal,
                valueP 'C' AlnOptUInt8Array   decimal,
                valueP 's' AlnOptInt16Array $ signed decimal,
                valueP 'S' AlnOptUInt16Array  decimal,
                valueP 'i' AlnOptInt32Array $ signed decimal,
                valueP 'I' AlnOptUInt32Array  decimal,
                valueP 'f' AlnOptFloatArray $ double2Float <$> signed double
               ]
  where
    valueP :: Char -> ([a] -> AlnOptValue) -> Parser a -> Parser AlnOptValue
    valueP c t p = char c *> (t <$> many (char ',' *> p))

optP :: Parser [AlnOpt]
optP = many $ tabP *> foldl1 (<|>) [optCharP,
                                    optIntP,
                                    optFloatP,
                                    optStringP,
                                    optByteArrayP,
                                    optArrayP]

restoreLongCigars :: Aln -> Aln
restoreLongCigars aln = maybe aln updateAln optCigars
  where
    (optCigars, restOpts) = findOptCigars $ aln ^. opt

    seqLen :: Int
    seqLen = maybe err B8.length $ aln ^. seq
      where err = error "sequence is empty. cannot determine the length of the sequence"

    isCigarDummy :: Maybe [CIG.Cigar] -> Bool
    isCigarDummy (Just (CIG.Cigar seqLen CIG.SoftClip:_)) = True
    isCigarDummy _ = False

    updateAln :: [CIG.Cigar] -> Aln
    updateAln c = if isCigarDummy $ aln ^. cigars
                    then aln & cigars .~ Just c
                             & opt    .~ restOpts
                    else error "cannot replace CIGAR field with CIGARs in optional \"CG\" field"

-- | find an optional CG tag from optional fields and returns CIGARs and rest of optional fields
findOptCigars :: [AlnOpt] -> (Maybe [CIG.Cigar], [AlnOpt])
findOptCigars [] = (Nothing, [])
findOptCigars (AlnOpt "CG" (AlnOptUInt32Array values):xs) = (Just $ map decodeOptCigar values, xs)
findOptCigars (AlnOpt "CG" _:_) = error "\"CG\" tag should have UInt32 Array as its value"
findOptCigars (x:xs) = (cs, x:xs') where (cs, xs') = findOptCigars xs

decodeOptCigar :: Word32 -> CIG.Cigar
decodeOptCigar x = CIG.Cigar (fromIntegral $ x `shiftR` 4) op
  where
    op = case x .&. 0xf of
             0 -> CIG.Match
             1 -> CIG.Ins
             2 -> CIG.Del
             3 -> CIG.Skip
             4 -> CIG.SoftClip
             5 -> CIG.HardClip
             6 -> CIG.Padding
             7 -> CIG.Equal
             8 -> CIG.NotEqual

samParser :: Parser Sam
samParser = Sam <$>
            headerParser <*>
            (restoreLongCigars <$> alnParser) `sepBy` many endOfLine <*
            option () (endOfLine <* skipSpace) -- the last EOL is not necessarily required
            <* endOfInput
