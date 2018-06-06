{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Sam
where

import Data.ByteString.Char8
import Data.Int
import Data.Word
import Bio.Sam.Cigar
import Control.Lens
import Data.Bits
import Data.Default
import Data.Sequence
import Data.Time.Clock
import Data.Time.ISO8601
import GHC.Generics

-- @CO (comment) lines are skipped
-- for the list of tags, see https://samtools.github.io/hts-specs/SAMtags.pdf

data SortOrder = UnknownOrder    |
                 UnsortedOrder   |
                 QueryNameOrder  |
                 CoordinateOrder
               deriving (Eq, Show)

data Grouping  = NoGroup        |
                 QueryGroup     |
                 ReferenceGroup
               deriving (Eq, Show)

data Platform  = Capillary  |
                 LS454      |
                 Illumina   |
                 SOLiD      |
                 Helicos    |
                 IonTorrent |
                 ONT        |
                 PacBio
               deriving (Eq, Show)

data RawField = RawField {
  _tagName  :: !String,
  _tagValue :: !String
  } deriving (Default, Generic, Show)

makeLenses ''RawField

data Reference = Reference {
  _refName      :: !String,
  _refLen       :: !Int,
  -- altLocus: Nothing         -> is not an alternative,
  --           Just (Nothing)  -> is an alternative and the locus is unknown,
  --           Just (Just ...) -> is an alternative and the locus is known
  -- may be needed to be implemented using original type
  _altLocus     :: !(Maybe (Maybe String)),
  _altRefNames  :: ![String],
  _assemblyID   :: !(Maybe String),
  _md5          :: !(Maybe String),
  _species      :: !(Maybe String),
  _uri          :: !(Maybe String),
  _refOptFields :: !(Seq RawField)
--  
  } deriving (Default, Generic, Show)

makeLenses ''Reference

data ReadGroup = ReadGroup {
  _readGroupID        :: !String,
  _sequencingCenter   :: !(Maybe String),
  _readGroupDesc      :: !(Maybe String), -- may be encoded in UTF-8 (but is currently unsupported)
  _date               :: !(Maybe UTCTime),
  -- for flowOrder and keySequence, see https://sourceforge.net/p/samtools/mailman/message/28536780/
  -- flowOrder: Nothing        -> information not available
  --            Just (Nothing) -> /*/
  --            Just (Nothing) -> /[ACMGRSVTWYHKDBN]+/
  _flowOrder          :: !(Maybe (Maybe String)),
  _keySequence        :: !(Maybe String),
  _library            :: !(Maybe String),
  _program            :: !(Maybe String), -- ??? should contain program "name" or program "ID" ???
  _medianInsert       :: !(Maybe Int),
  _platform           :: !(Maybe Platform),  
  _platformModel      :: !(Maybe String),
  _platformUnit       :: !(Maybe String),
  _readGroupOptFields :: !(Seq RawField),
  _sample             :: !(Maybe String)
  } deriving (Default, Generic, Show)

makeLenses ''ReadGroup

data Program = Program {
  _programID        :: !String,
  _programName      :: !(Maybe String),
  _commandLine      :: !(Maybe String), -- may be encoded in UTF-8 (but is currently unsupported)
  _prevProgramID    :: !(Maybe String),
  _programDesc      :: !(Maybe String),
  _programVersion   :: !(Maybe String),
  _programOptFields :: !(Seq RawField)
  } deriving (Default, Generic, Show)

makeLenses ''Program

data Header = Header {
  _version         :: !(Maybe String),
  _sortOrder       :: !(Maybe SortOrder),
  _grouping        :: !(Maybe Grouping),
  _optHeaderFields :: !(Seq RawField),
  _references      :: !(Seq Reference),
  _readGroups      :: !(Seq ReadGroup),
  _programs        :: !(Seq Program)
  } deriving (Default, Generic, Show)

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
  AlnOptString      String     |
  AlnOptByteArray   ByteString |
  AlnOptInt8Array   [Int8]     |
  AlnOptUInt8Array  [Word8]    |
  AlnOptInt16Array  [Int16]    |
  AlnOptUInt16Array [Word16]   |
  AlnOptInt32Array  [Int32]    |
  AlnOptUInt32Array [Word32]   |
  AlnOptFloatArray  [Float]
  deriving Show

data AlnOpt = AlnOpt {
  _alnOptTag   :: String,
  _alnOptValue :: AlnOptValue
  } deriving Show

makeLenses ''AlnOpt

data Aln = Aln {
  _qname  :: !String,
  _flag   :: !Word16,
  _rname  :: !(Maybe String),
  _pos    :: !(Maybe Word32),
  _mapq   :: !(Maybe Word8),
  _cigars :: !(Maybe [Cigar]),
  _rnext  :: !(Maybe String),
  _pnext  :: !(Maybe Word32),
  _tlen   :: !Int32,
  _seq    :: !(Maybe String),
  _qual   :: !(Maybe String),
  _opt    :: ![AlnOpt]
  } deriving Show

makeLenses ''Aln

data Sam = Sam {
  _header :: Header,
  _alns   :: [Aln]
  } deriving Show

makeLenses ''Sam

