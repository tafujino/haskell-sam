{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Bio.Sam.SamParser
  (samParser
  )
where

import Prelude hiding (seq, takeWhile)
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((|>))
import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Default
import Data.Sequence hiding (null, length)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base16
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import GHC.Float
import Bio.Sam.Sam
import qualified Bio.Sam.Cigar as C

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&>  

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||>

asciiRange :: Char -> Char -> Char -> Bool
asciiRange x y = (x <=) <&&> (<= y)

(<->) :: Char -> Char -> Char -> Bool
(<->) = asciiRange
infix 4 <->

-- | possibly faster implementation of inClass (range notation is unsupported)
inClass' :: String -> Char -> Bool
inClass' = foldl1 (<||>) . map (==)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
infixr 5 <++>

-- functions for testing bitwise flags
testFlag :: Int -> Aln -> Bool
testFlag n aln = testBit (aln ^. flag) n

hasMultipleSegments :: Aln -> Bool
hasMultipleSegments = testFlag 0x1

isProperlyAligned :: Aln -> Bool
isProperlyAligned = testFlag 0x2

isUnmapped :: Aln -> Bool
isUnmapped = testFlag 0x4

isNextUnmapped :: Aln -> Bool
isNextUnmapped = testFlag 0x8

isReverseComplement :: Aln -> Bool
isReverseComplement = testFlag 0x10

isNextReverseComplement :: Aln -> Bool
isNextReverseComplement = testFlag 0x20

isFirstSegment :: Aln -> Bool
isFirstSegment = testFlag 0x40

isLastSegment :: Aln -> Bool
isLastSegment = testFlag 0x80

isSecondary :: Aln -> Bool
isSecondary = testFlag 0x100

hasNotPassedFilters :: Aln -> Bool
hasNotPassedFilters = testFlag 0x200

isDuplicate :: Aln -> Bool
isDuplicate = testFlag 0x400

isSupplementary :: Aln -> Bool
isSupplementary = testFlag 0x800

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

isHexChar :: Char -> Bool
isHexChar = isDigit <||> 'A' <-> 'F' <||> 'a' <-> 'f'

isNucleotideBaseChar :: Char -> Bool
isNucleotideBaseChar = inClass' "ACMGRSVTWYHKDBN"  

anyFieldChar :: Char -> Bool
anyFieldChar = (/= '\t') <&&> (/= '\r') <&&> (/= '\n')

anyFieldStringP :: Parser String
anyFieldStringP = B8.unpack <$> takeWhile1 anyFieldChar

rawFieldP :: Parser RawField
rawFieldP = tabP *> (RawField <$> count 2 anyChar <* char ':' <*> anyFieldStringP)

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
headerEmptyLineP = (endOfLine $>)

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

headerTagValueP :: String -> Parser a -> Parser a
headerTagValueP tag = (tabP *> string (B8.pack tag) *> char ':' *>)

headerFieldP :: Lens' a b -> (b -> Bool) -> String -> Parser b -> a -> Parser a
headerFieldP loc cond tag parser state = headerTagValueP tag $ do
  x <- parser
  unless (cond $ state ^. loc) $ error $ "tag " ++ tag ++ "appeared twice"
  return $ state & loc .~ x

headerMaybeFieldP :: Lens' a (Maybe b) -> String -> Parser b -> a -> Parser a
headerMaybeFieldP loc tag parser = headerFieldP loc isNothing tag $ Just <$> parser

headerListFieldP :: Lens' a [b] -> String -> Parser [b] -> a -> Parser a
headerListFieldP loc = headerFieldP loc null

headerStringFieldP :: Lens' a String -> String -> Parser String -> a -> Parser a
headerStringFieldP = headerListFieldP

headerRawFieldP :: Lens' a (Seq RawField) -> a -> Parser a
headerRawFieldP loc state = do
  x <- rawFieldP
  return $ state & loc %~ (|> x)
  
enumP :: [(String, a)] -> Parser a
enumP = foldl1 (<|>) . map (\(str, val) -> string (B8.pack str) $> val)

versionP :: Header -> Parser Header
versionP = headerMaybeFieldP version "VN" $ digitString <++> char '.' <:> digitString
  where digitString = B8.unpack <$> takeWhile1 isDigit

sortOrderP :: Header -> Parser Header
sortOrderP = headerFieldP sortOrder isNothing "SO" $
             Just <$> enumP [("unknown",    UnknownOrder   ),
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
                       ((^. refName . to (not . null)) <&&> (^. refLen . to (/= 0)))
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
refSeqNameP = headerStringFieldP refName "SN" $
              satisfy ('!' <-> ')' <||> '+' <-> '<' <||> '>' <-> '~') <:>
              (B8.unpack <$> takeWhile ('!' <-> '~'))

refLenP :: Reference -> Parser Reference
refLenP = headerFieldP refLen (== 0) "LN" decimal

refAltLocusP :: Reference -> Parser Reference
refAltLocusP = headerMaybeFieldP altLocus "AH" $ starOr (Just <$> anyFieldStringP)

refAltRefNamesP :: Reference -> Parser Reference
refAltRefNamesP = headerListFieldP altRefNames "AN" $ flip sepBy1 (char ',') $
                  satisfy (isDigit <||> isAlpha_ascii) <:>
                  (B8.unpack <$> takeWhile (isDigit <||> isAlpha_ascii <||> inClass' "*+.@_|-"))

refAssemblyIDP :: Reference -> Parser Reference
refAssemblyIDP = headerMaybeFieldP assemblyID "AS" anyFieldStringP

refMD5P :: Reference -> Parser Reference
refMD5P = headerMaybeFieldP md5 "M5" $ B8.unpack <$> takeWhile isHexChar

refSpeciesP :: Reference -> Parser Reference
refSpeciesP = headerMaybeFieldP species "SP" anyFieldStringP

refURIP :: Reference -> Parser Reference
refURIP = headerMaybeFieldP uri "UR" anyFieldStringP -- for now, accepts any non-empty string

refOptFieldP :: Reference -> Parser Reference
refOptFieldP = headerRawFieldP refOptFields

-----

headerReadGroupLineP :: HeaderState -> Parser HeaderState
headerReadGroupLineP = headerLineP
                       "RG"
                       (const True)
                       (^. readGroupID . to (not . null))
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
readGroupIDP = headerStringFieldP readGroupID "ID" anyFieldStringP

readGroupSeqCenterP :: ReadGroup -> Parser ReadGroup
readGroupSeqCenterP = headerMaybeFieldP sequencingCenter "CN" anyFieldStringP

readGroupDescP :: ReadGroup -> Parser ReadGroup
readGroupDescP = headerMaybeFieldP readGroupDesc "DS" anyFieldStringP

-- currently does not check format (ISO8601 or date/time)
-- should use Data.Time.ISO8601
readGroupDateP :: ReadGroup -> Parser ReadGroup
readGroupDateP = headerMaybeFieldP date "DT" anyFieldStringP

readGroupFlowOrderP :: ReadGroup -> Parser ReadGroup
readGroupFlowOrderP = headerMaybeFieldP flowOrder "FO" $
                      starOr $ Just <$> many1 (satisfy isNucleotideBaseChar)

readGroupKeySeqP :: ReadGroup -> Parser ReadGroup
readGroupKeySeqP = headerMaybeFieldP keySequence "KS" $ many1 (satisfy isNucleotideBaseChar)

readGroupLibraryP :: ReadGroup -> Parser ReadGroup
readGroupLibraryP = headerMaybeFieldP keySequence "LB" anyFieldStringP

readGroupProgramP :: ReadGroup -> Parser ReadGroup
readGroupProgramP = headerMaybeFieldP program "PG" anyFieldStringP

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
readGroupPlatformModelP = headerMaybeFieldP platformModel "PM" anyFieldStringP

readGroupPlatformUnitP :: ReadGroup -> Parser ReadGroup
readGroupPlatformUnitP = headerMaybeFieldP platformUnit "PU" anyFieldStringP

readGroupSampleP :: ReadGroup -> Parser ReadGroup
readGroupSampleP = headerMaybeFieldP sample "SM" anyFieldStringP

readGroupOptFieldP :: ReadGroup -> Parser ReadGroup
readGroupOptFieldP = headerRawFieldP readGroupOptFields

-----

headerProgramLineP :: HeaderState -> Parser HeaderState
headerProgramLineP = headerLineP
                     "PG"
                     (const True)
                     (^. programID . to (not . null))
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
programIDP = headerStringFieldP programID "ID" anyFieldStringP

programNameP :: Program -> Parser Program
programNameP = headerMaybeFieldP programName "PN" anyFieldStringP

programCmdLineP :: Program -> Parser Program
programCmdLineP = headerMaybeFieldP commandLine "CL" anyFieldStringP

programPrevIDP :: Program -> Parser Program
programPrevIDP = headerMaybeFieldP prevProgramID "PP" anyFieldStringP

programDescP :: Program -> Parser Program
programDescP = headerMaybeFieldP programDesc "DS" anyFieldStringP

programVersionP :: Program -> Parser Program
programVersionP = headerMaybeFieldP programVersion "VN" anyFieldStringP

programOptFieldP :: Program -> Parser Program
programOptFieldP = headerRawFieldP programOptFields

-----

headerCommentLineP :: HeaderState -> Parser HeaderState
headerCommentLineP = headerLineP
                     "CO"
                     (const True)
                     (const True)
                     (flip const)
                     [(tabP *> A.skipWhile isEndOfLine $>) :: Header -> Parser Header]

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

qnameP :: Parser String
qnameP = B8.unpack <$> takeWhile ('!' <-> '?' <||> 'A' <-> '~')

flagP :: Parser Word16
flagP = decimal

rnameP :: Parser (Maybe String)
rnameP = starOr $ Just <$>
         satisfy ('!' <-> '<' <||> '>' <-> '~') <:> (B8.unpack <$> takeWhile ('!' <-> '~'))

posP :: Parser (Maybe Word32)
posP = starOr $ validIf (/= 0) <$> decimal

mapqP :: Parser (Maybe Word8)
mapqP = starOr $ Just <$> decimal

cigar1P :: Parser C.Cigar
cigar1P = C.Cigar <$> decimal <*> (charToCigar <$> satisfy (inClass' "MIDNSHP=X"))
  where
    charToCigar chr = case chr of
      'M' -> C.Match
      'I' -> C.Ins
      'D' -> C.Del
      'N' -> C.Skip
      'S' -> C.SoftClip
      'H' -> C.HardClip
      'P' -> C.Padding
      '=' -> C.Equal
      'X' -> C.NotEqual
      _   -> error "invalid C.Cigar op char"

cigarsP :: Parser (Maybe [C.Cigar])
cigarsP = starOr $ Just <$> many cigar1P

rnextP :: Parser (Maybe String)
rnextP = Just . B8.unpack <$> "=" <|>
         rnameP

pnextP :: Parser (Maybe Word32)
pnextP = starOr $ validIf (/= 0) <$> decimal

tlenP :: Parser Int32
tlenP = signed decimal

seqP :: Parser (Maybe String)
seqP = starOr $ Just . B8.unpack <$> takeWhile1 (isAlpha_ascii <||> (== '=') <||> (== '.'))

qualP :: Parser (Maybe String)
qualP = starOr $ Just . B8.unpack <$> takeWhile1 ('!' <-> '~')

opt1P :: Char -> Parser AlnOptValue -> Parser AlnOpt
opt1P c p = AlnOpt <$> tagP <* char ':' <* char c <* char ':' <*> p
  where tagP = satisfy isAlpha_ascii <:> satisfy (isAlpha_ascii <||> isDigit) <:> pure []

optCharP :: Parser AlnOpt
optCharP = opt1P 'A' $ AlnOptChar <$> anyChar

optIntP :: Parser AlnOpt
optIntP = opt1P 'i' $ AlnOptInt32 <$> signed decimal

optFloatP :: Parser AlnOpt
optFloatP = opt1P 'f' $ AlnOptFloat . double2Float <$> signed double

optStringP :: Parser AlnOpt
optStringP = opt1P 'Z' $ AlnOptString . B8.unpack <$> takeWhile (' ' <-> '~')

optByteArrayP :: Parser AlnOpt
optByteArrayP = opt1P 'H' $ do
  (result, invalid) <- decode <$> takeWhile isHexChar
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
    seqLen = maybe err length $ aln ^. seq 
      where err = error "sequence is empty. cannot determine the length of the sequence"

    isCigarDummy :: Maybe [C.Cigar] -> Bool
    isCigarDummy (Just (C.Cigar seqLen C.SoftClip:_)) = True
    isCigarDummy _ = False
    
    updateAln :: [C.Cigar] -> Aln
    updateAln c = if isCigarDummy $ aln ^. cigars 
                    then aln & cigars .~ Just c
                             & opt    .~ restOpts 
                    else error "cannot replace CIGAR field with CIGARs in optional \"CG\" field"

-- | find an optional CG tag from optional fields and returns CIGARs and rest of optional fields
findOptCigars :: [AlnOpt] -> (Maybe [C.Cigar], [AlnOpt])
findOptCigars [] = (Nothing, [])
findOptCigars (AlnOpt "CG" (AlnOptUInt32Array values):xs) = (Just $ map decodeOptCigar values, xs)
findOptCigars (AlnOpt "CG" _:_) = error "\"CG\" tag should have UInt32 Array as its value"
findOptCigars (x:xs) = (cs, x:xs') where (cs, xs') = findOptCigars xs

decodeOptCigar :: Word32 -> C.Cigar
decodeOptCigar x = C.Cigar (fromIntegral $ x `shiftR` 4) op
  where
    op = case x .&. 0xf of
             0 -> C.Match
             1 -> C.Ins
             2 -> C.Del
             3 -> C.Skip
             4 -> C.SoftClip
             5 -> C.HardClip
             6 -> C.Padding
             7 -> C.Equal
             8 -> C.NotEqual

samParser :: Parser Sam
samParser = Sam <$>
            headerParser <*>
            (restoreLongCigars <$> alnParser) `sepBy` many endOfLine <*
            option () endOfLine -- the last EOL is not necessarily required
            <* endOfInput 

