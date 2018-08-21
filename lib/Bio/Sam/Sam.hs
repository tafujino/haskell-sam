{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Sam
where

import Data.ByteString.Char8
import Data.Int
import Data.Word
import Bio.Sam.Cigar
import Control.Lens
import Control.DeepSeq
import Data.Bits
import Data.Default
import Data.Sequence
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Text
import GHC.Generics

-- @CO (comment) lines are skipped
-- for the list of tags, see https://samtools.github.io/hts-specs/SAMtags.pdf

data SortOrder = UnknownOrder    |
                 UnsortedOrder   |
                 QueryNameOrder  |
                 CoordinateOrder
               deriving (Eq, Generic, Show)

instance NFData SortOrder

data Grouping = NoGroup        |
                QueryGroup     |
                ReferenceGroup
              deriving (Eq, Generic, Show)

instance NFData Grouping

data Platform = Capillary  |
                LS454      |
                Illumina   |
                SOLiD      |
                Helicos    |
                IonTorrent |
                ONT        |
                PacBio
              deriving (Eq, Generic, Show)

instance NFData Platform

data RawField = RawField {
  _tagName  :: !ByteString,
  _tagValue :: !ByteString
  } deriving (Default, Show, Generic)

instance NFData RawField
makeLenses ''RawField

data Reference = Reference {
  _refName      :: !ByteString,
  _refLen       :: !Int,
  -- altLocus: Nothing         -> is not an alternative,
  --           Just (Nothing)  -> is an alternative and the locus is unknown,
  --           Just (Just ...) -> is an alternative and the locus is known
  -- may be needed to be implemented using original type
  _altLocus     :: !(Maybe (Maybe ByteString)),
  _altRefNames  :: ![ByteString],
  _assemblyID   :: !(Maybe ByteString),
  _md5          :: !(Maybe ByteString),
  _species      :: !(Maybe ByteString),
  _uri          :: !(Maybe ByteString),
  _refOptFields :: !(Seq RawField)
--  
  } deriving (Default, Generic, Show)

instance NFData Reference
makeLenses ''Reference

data ReadGroup = ReadGroup {
  _readGroupID        :: !ByteString,
  _sequencingCenter   :: !(Maybe ByteString),
  _readGroupDesc      :: !(Maybe Text), -- may be encoded in UTF-8 (but is currently unsupported)
  _date               :: !(Maybe UTCTime),
  -- for flowOrder and keySequence, see https://sourceforge.net/p/samtools/mailman/message/28536780/
  -- flowOrder: Nothing        -> information not available
  --            Just (Nothing) -> /*/
  --            Just (Nothing) -> /[ACMGRSVTWYHKDBN]+/
  _flowOrder          :: !(Maybe (Maybe ByteString)),
  _keySequence        :: !(Maybe ByteString),
  _library            :: !(Maybe ByteString),
  _program            :: !(Maybe ByteString), -- ??? should contain program "name" or program "ID" ???
  _medianInsert       :: !(Maybe Int),
  _platform           :: !(Maybe Platform),  
  _platformModel      :: !(Maybe ByteString),
  _platformUnit       :: !(Maybe ByteString),
  _readGroupOptFields :: !(Seq RawField),
  _sample             :: !(Maybe ByteString)
  } deriving (Default, Generic, Show)

instance NFData ReadGroup
makeLenses ''ReadGroup

data Program = Program {
  _programID        :: !ByteString,
  _programName      :: !(Maybe ByteString),
  _commandLine      :: !(Maybe Text), -- may be encoded in UTF-8 (but is currently unsupported)
  _prevProgramID    :: !(Maybe ByteString),
  _programDesc      :: !(Maybe Text),
  _programVersion   :: !(Maybe ByteString),
  _programOptFields :: !(Seq RawField)
  } deriving (Default, Generic, Show)

instance NFData Program
makeLenses ''Program

data Header = Header {
  _version         :: !(Maybe ByteString),
  _sortOrder       :: !(Maybe SortOrder),
  _grouping        :: !(Maybe Grouping),
  _optHeaderFields :: !(Seq RawField),
  _references      :: !(Seq Reference),
  _readGroups      :: !(Seq ReadGroup),
  _programs        :: !(Seq Program)
  } deriving (Default, Generic, Show)

instance NFData Header
makeLenses ''Header

data AlnOptValue =
  AlnOptChar        Char       |
  AlnOptInt         Int8       | -- only for BAM
  AlnOptUInt8       Word8      | -- only for BAM
  AlnOptInt16       Int16      | -- only for BAM
  AlnOptUInt16      Word16     | -- only for BAM
  AlnOptInt32       Int32      |
  AlnOptUInt32      Word32     | -- only for BAM
  AlnOptFloat       Float      |
  AlnOptString      ByteString |
  AlnOptByteArray   ByteString |
  AlnOptInt8Array   [Int8]     |
  AlnOptUInt8Array  [Word8]    |
  AlnOptInt16Array  [Int16]    |
  AlnOptUInt16Array [Word16]   |
  AlnOptInt32Array  [Int32]    |
  AlnOptUInt32Array [Word32]   |
  AlnOptFloatArray  [Float]
  deriving (Generic, Show)

instance NFData AlnOptValue

data AlnOpt = AlnOpt {
  _alnOptTag   :: ByteString,
  _alnOptValue :: AlnOptValue
  } deriving (Generic, Show)

instance NFData AlnOpt
makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !ByteString,
  _flag   :: !Word16,
  _rname  :: !(Maybe ByteString),
  _pos    :: !(Maybe Word32), -- change to Int64
  _mapq   :: !(Maybe Word8),
  _cigars :: !(Maybe [Cigar]),
  _rnext  :: !(Maybe ByteString),
  _pnext  :: !(Maybe Word32),
  _tlen   :: !Int32,
  _seq    :: !(Maybe ByteString),
  _qual   :: !(Maybe ByteString),
  _opt    :: ![AlnOpt]
  } deriving (Generic, Show)

instance NFData Aln
makeLenses ''Aln

data Sam = Sam {
  _header :: Header,
  _alns   :: [Aln]
  } deriving (Generic, Show)

instance NFData Sam
makeLenses ''Sam

