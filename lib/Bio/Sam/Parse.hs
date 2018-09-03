{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Bio.Sam.Parse
  (rawSamParser,
   samParser
  )
where

import Prelude hiding (take, takeWhile)
import qualified Bio.Sam.Cigar as CIG
import qualified Bio.Sam.Header as H
import qualified Bio.Sam.RawSam as R
import qualified Bio.Sam as S
import Bio.Sam.Convert
import Control.Applicative
import Control.Arrow
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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Word
import Data.Attoparsec.Applicative
import Data.Attoparsec.ByteString.Char8
import GHC.Float

type LineNo = Int
data HeaderState = HeaderState {
  runHeader :: H.Header,
  runLineNo :: LineNo
  } deriving Show

-- =============================================================================
-- header parser
-- =============================================================================

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

rawFieldP :: Parser H.RawField
rawFieldP = tabP *> (H.RawField <$> take 2 <* char ':' <*> anyFieldByteStringP)

headerLineP :: (Default a) =>
               String ->
               (LineNo -> Bool) ->
               (a -> Bool) ->
               (a -> H.Header -> H.Header) ->
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

--------------------------------------------------------------------------------

headerEmptyLineP :: HeaderState -> Parser HeaderState
headerEmptyLineP (HeaderState header lineNo) = skipSpace *> endOfLine $> HeaderState header (lineNo + 1)

--------------------------------------------------------------------------------

headerTopLineP :: HeaderState -> Parser HeaderState
headerTopLineP = headerLineP
                 "HD"
                 (== 1)
                 (^. H.version . to isJust)
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

headerRawFieldP :: Lens' a (Seq H.RawField) -> a -> Parser a
headerRawFieldP loc state = do
  x <- rawFieldP
  return $ state & loc %~ (|> x)

enumP :: [(String, a)] -> Parser a
enumP = foldl1 (<|>) . map (\(str, val) -> string (B8.pack str) $> val)

versionP :: H.Header -> Parser H.Header
versionP = headerMaybeFieldP H.version "VN" $ do
  xs  <- digitString
  dot <- char '.'
  ys  <- digitString
  return $ xs `B8.append` (dot `B8.cons` ys)
  where digitString = takeWhile1 isDigit

sortOrderP :: H.Header -> Parser H.Header
sortOrderP = headerMaybeFieldP H.sortOrder "SO" $ enumP [("unknown",    H.UnknownOrder   ),
                                                         ("unsorted",   H.UnsortedOrder  ),
                                                         ("queryname",  H.QueryNameOrder ),
                                                         ("coordinate", H.CoordinateOrder)]

groupingP :: H.Header -> Parser H.Header
groupingP = headerMaybeFieldP H.grouping "GO" $ enumP [("none",      H.NoGroup       ),
                                                       ("query",     H.QueryGroup    ),
                                                       ("reference", H.ReferenceGroup)]

optHeaderFieldP :: H.Header -> Parser H.Header
optHeaderFieldP = headerRawFieldP H.optHeaderFields

--------------------------------------------------------------------------------

headerReferenceLineP :: HeaderState -> Parser HeaderState
headerReferenceLineP = headerLineP
                       "SQ"
                       (const True)
                       ((^. H.refName . to (not . B8.null)) <&&> (^. H.refLen . to (/= 0)))
                       (appendAt H.references)
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

refSeqNameP :: H.Reference -> Parser H.Reference
refSeqNameP = headerByteStringFieldP H.refName "SN" $
              liftA2 B8.cons (satisfy ('!' <-> ')' <||> '+' <-> '<' <||> '>' <-> '~')) (takeWhile ('!' <-> '~'))

refLenP :: H.Reference -> Parser H.Reference
refLenP = headerFieldP H.refLen (== 0) "LN" decimal

refAltLocusP :: H.Reference -> Parser H.Reference
refAltLocusP = headerMaybeFieldP H.altLocus "AH" $ starOr (Just <$> anyFieldByteStringP)

refAltRefNamesP :: H.Reference -> Parser H.Reference
refAltRefNamesP = headerListFieldP H.altRefNames "AN" $ (`sepBy1` char ',') $
                  liftA2 B8.cons
                  (satisfy   (isDigit <||> isAlpha_ascii))
                  (takeWhile (isDigit <||> isAlpha_ascii <||> inClass' "*+.@_|-"))

refAssemblyIDP :: H.Reference -> Parser H.Reference
refAssemblyIDP = headerMaybeFieldP H.assemblyID "AS" anyFieldByteStringP

refMD5P :: H.Reference -> Parser H.Reference
refMD5P = headerMaybeFieldP H.md5 "M5" $ takeWhile isHex

refSpeciesP :: H.Reference -> Parser H.Reference
refSpeciesP = headerMaybeFieldP H.species "SP" anyFieldByteStringP

refURIP :: H.Reference -> Parser H.Reference
refURIP = headerMaybeFieldP H.uri "UR" anyFieldByteStringP -- for now, accepts any non-empty bytestring

refOptFieldP :: H.Reference -> Parser H.Reference
refOptFieldP = headerRawFieldP H.refOptFields

--------------------------------------------------------------------------------

headerReadGroupLineP :: HeaderState -> Parser HeaderState
headerReadGroupLineP = headerLineP
                       "RG"
                       (const True)
                       (^. H.readGroupID . to (not . B8.null))
                       (appendAt H.readGroups)
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

readGroupIDP :: H.ReadGroup -> Parser H.ReadGroup
readGroupIDP = headerByteStringFieldP H.readGroupID "ID" anyFieldByteStringP

readGroupSeqCenterP :: H.ReadGroup -> Parser H.ReadGroup
readGroupSeqCenterP = headerMaybeFieldP H.sequencingCenter "CN" anyFieldByteStringP

readGroupDescP :: H.ReadGroup -> Parser H.ReadGroup
readGroupDescP = headerMaybeFieldP H.readGroupDesc "DS" anyFieldTextP

readGroupDateP :: H.ReadGroup -> Parser H.ReadGroup
readGroupDateP = headerMaybeFieldP H.date "DT" $ maybe mzero return =<< parseISO8601 . B8.unpack <$> anyFieldByteStringP

readGroupFlowOrderP :: H.ReadGroup -> Parser H.ReadGroup
readGroupFlowOrderP = headerMaybeFieldP H.flowOrder "FO" $
                      starOr $ Just . B8.pack <$> many1 (satisfy isNucleotideBaseChar)

readGroupKeySeqP :: H.ReadGroup -> Parser H.ReadGroup
readGroupKeySeqP = headerMaybeFieldP H.keySequence "KS" $ B8.pack <$> many1 (satisfy isNucleotideBaseChar)

readGroupLibraryP :: H.ReadGroup -> Parser H.ReadGroup
readGroupLibraryP = headerMaybeFieldP H.keySequence "LB" anyFieldByteStringP

readGroupProgramP :: H.ReadGroup -> Parser H.ReadGroup
readGroupProgramP = headerMaybeFieldP H.program "PG" anyFieldByteStringP

readGroupMedianInsertP :: H.ReadGroup -> Parser H.ReadGroup
readGroupMedianInsertP = headerMaybeFieldP H.medianInsert "PI" decimal

readGroupPlatformP :: H.ReadGroup -> Parser H.ReadGroup
readGroupPlatformP = headerMaybeFieldP H.platform "PL" $ enumP [("CAPILLARY",  H.Capillary ),
                                                                ("LS454",      H.LS454     ),
                                                                ("ILLUMINA",   H.Illumina  ),
                                                                ("SOLID",      H.SOLiD     ),
                                                                ("HELICOS",    H.Helicos   ),
                                                                ("IONTORRENT", H.IonTorrent),
                                                                ("ONT",        H.ONT       ),
                                                                ("PACBIO",     H.PacBio    )
                                                               ]

readGroupPlatformModelP :: H.ReadGroup -> Parser H.ReadGroup
readGroupPlatformModelP = headerMaybeFieldP H.platformModel "PM" anyFieldByteStringP

readGroupPlatformUnitP :: H.ReadGroup -> Parser H.ReadGroup
readGroupPlatformUnitP = headerMaybeFieldP H.platformUnit "PU" anyFieldByteStringP

readGroupSampleP :: H.ReadGroup -> Parser H.ReadGroup
readGroupSampleP = headerMaybeFieldP H.sample "SM" anyFieldByteStringP

readGroupOptFieldP :: H.ReadGroup -> Parser H.ReadGroup
readGroupOptFieldP = headerRawFieldP H.readGroupOptFields

--------------------------------------------------------------------------------

headerProgramLineP :: HeaderState -> Parser HeaderState
headerProgramLineP = headerLineP
                     "PG"
                     (const True)
                     (^. H.programID . to (not . B8.null))
                     (appendAt H.programs)
                     [programIDP,
                      programNameP,
                      programCmdLineP,
                      programPrevIDP,
                      programDescP,
                      programVersionP,
                      programOptFieldP
                     ]

programIDP :: H.Program -> Parser H.Program
programIDP = headerByteStringFieldP H.programID "ID" anyFieldByteStringP

programNameP :: H.Program -> Parser H.Program
programNameP = headerMaybeFieldP H.programName "PN" anyFieldByteStringP

programCmdLineP :: H.Program -> Parser H.Program
programCmdLineP = headerMaybeFieldP H.commandLine "CL" anyFieldTextP

programPrevIDP :: H.Program -> Parser H.Program
programPrevIDP = headerMaybeFieldP H.prevProgramID "PP" anyFieldByteStringP

programDescP :: H.Program -> Parser H.Program
programDescP = headerMaybeFieldP H.programDesc "DS" anyFieldTextP

programVersionP :: H.Program -> Parser H.Program
programVersionP = headerMaybeFieldP H.programVersion "VN" anyFieldByteStringP

programOptFieldP :: H.Program -> Parser H.Program
programOptFieldP = headerRawFieldP H.programOptFields

--------------------------------------------------------------------------------

headerCommentLineP :: HeaderState -> Parser HeaderState
headerCommentLineP = headerLineP
                     "CO"
                     (const True)
                     (const True)
                     (flip const)
                     [(tabP *> takeTill isEndOfLine' $>) :: H.Header -> Parser H.Header]

--------------------------------------------------------------------------------

-- should check uniqueness of read group ID
-- should check if @PG-PP refers to any @PG-ID
headerParser :: Parser H.Header
headerParser = runHeader <$>
               updateMany
               (updateAlt [headerTopLineP,
                           headerReferenceLineP,
                           headerReadGroupLineP,
                           headerProgramLineP,
                           headerCommentLineP,
                           headerEmptyLineP])
               (HeaderState def 1)

-- =============================================================================
-- alignment parser
-- =============================================================================

alnParser :: Parser R.Aln
alnParser = do
  alnWithoutOpt <- R.Aln <$>
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
                   qualP
  (opt, mLongCigars) <- optP
  return $ fillLongCigars (alnWithoutOpt opt) mLongCigars

fillLongCigars :: R.Aln -> Maybe (V.Vector CIG.Cigar) -> R.Aln
fillLongCigars aln Nothing           = aln
fillLongCigars aln (Just longCigars) =
  if hasDummyCigars aln
    then aln & R.cigars .~ Just longCigars
    else error "cannot replace CIGAR field with CIGARs in optional \"CG\" field"

hasDummyCigars :: R.Aln -> Bool
hasDummyCigars aln =
  case aln ^. R.cigars of
    Just cigars -> not (null cigars) && V.head cigars == CIG.Cigar seqLen CIG.SoftClip
      where
        seqLen = maybe err B8.length $ aln ^. R.seq
        err = error "sequence is empty. cannot determine the length of the sequence"
    Nothing -> False




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

cigarsP :: Parser (Maybe (V.Vector CIG.Cigar))
cigarsP = starOr $ Just . V.fromList <$> many cigar1P

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

opt1P :: Char -> Parser R.AlnOptValue -> Parser R.AlnOpt
opt1P c p = R.AlnOpt <$> tagP <* char ':' <* char c <* char ':' <*> p
  where tagP = B8.pack <$> satisfy isAlpha_ascii <:> satisfy (isAlpha_ascii <||> isDigit) <:> pure []

optCharP :: Parser R.AlnOpt
optCharP = opt1P 'A' $ R.AlnOptChar <$> anyChar

optIntP :: Parser R.AlnOpt
optIntP = opt1P 'i' $ R.AlnOptInt32 <$> signed decimal

optFloatP :: Parser R.AlnOpt
optFloatP = opt1P 'f' $ R.AlnOptFloat . double2Float <$> signed double

optStringP :: Parser R.AlnOpt
optStringP = opt1P 'Z' $ R.AlnOptString <$> takeWhile (' ' <-> '~')

optByteArrayP :: Parser R.AlnOpt
optByteArrayP = opt1P 'H' $ do
  (result, invalid) <- decode <$> takeWhile isHex
  -- function 'hexadecimal' outputs a variable of class Integral, but in this case
  -- outputting ByteString may be more preferable
  -- when invalid characters exist or the length is odd, this gives mzero (without error messages)
  -- see https://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Internal-Types.html#t:Parser
  guard $ B8.null invalid
  return $ R.AlnOptByteArray result

optArrayP :: Parser R.AlnOpt
optArrayP = opt1P 'B' $
  foldl1 (<|>) [valueP 'c' R.AlnOptInt8Array  $ signed decimal,
                valueP 'C' R.AlnOptUInt8Array   decimal,
                valueP 's' R.AlnOptInt16Array $ signed decimal,
                valueP 'S' R.AlnOptUInt16Array  decimal,
                valueP 'i' R.AlnOptInt32Array $ signed decimal,
                valueP 'I' R.AlnOptUInt32Array  decimal,
                valueP 'f' R.AlnOptFloatArray $ double2Float <$> signed double
               ]
  where
    valueP :: UV.Unbox a => Char -> (UV.Vector a -> R.AlnOptValue) -> Parser a -> Parser R.AlnOptValue
    valueP c t p = char c *> (t . UV.fromList <$> many (char ',' *> p))

optP :: Parser (V.Vector R.AlnOpt, Maybe (V.Vector CIG.Cigar))
optP = first V.fromList . findOptCigars <$> many (tabP *> foldl1 (<|>) [optCharP,
                                                                        optIntP,
                                                                        optFloatP,
                                                                        optStringP,
                                                                        optByteArrayP,
                                                                        optArrayP])

-- | find an optional CG tag from optional fields and returns CIGARs and rest of optional fields
findOptCigars :: [R.AlnOpt] -> ([R.AlnOpt], Maybe (V.Vector CIG.Cigar))
findOptCigars [] = ([], Nothing)
findOptCigars (R.AlnOpt "CG" (R.AlnOptUInt32Array values):xs) = (xs, Just $ V.map decodeOptCigar $ V.convert values)
findOptCigars (R.AlnOpt "CG" _:_) = error "\"CG\" tag should have UInt32 Array as its value"
findOptCigars (x:xs) = (x:xs', cs) where (xs', cs) = findOptCigars xs

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

rawSamParser :: Parser R.Sam
rawSamParser = R.Sam <$>
               headerParser <*>
               (V.fromList <$> alnParser `sepBy` many endOfLine) <* -- move this to RawSam -> Sam conversion stage
               option () (endOfLine <* skipSpace) -- the last EOL is not necessarily required
               <* endOfInput

samParser :: Parser S.Sam
samParser = fromRawSam <$> rawSamParser
