{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Sam.Header
where

import Data.ByteString.Char8
import Control.Lens
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

data Grouping = NoGroup        |
                QueryGroup     |
                ReferenceGroup
              deriving (Eq, Generic, Show)

data Platform = Capillary  |
                LS454      |
                Illumina   |
                SOLiD      |
                Helicos    |
                IonTorrent |
                ONT        |
                PacBio
              deriving (Eq, Generic, Show)

data RawField = RawField {
  _tagName  :: !ByteString,
  _tagValue :: !ByteString
  } deriving (Default, Show, Generic)

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
  } deriving (Default, Generic, Show)

makeLenses ''Reference

data ReadGroup = ReadGroup {
  _readGroupID        :: !ByteString,
  _sequencingCenter   :: !(Maybe ByteString),
  _readGroupDesc      :: !(Maybe Text),
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

makeLenses ''ReadGroup

data Program = Program {
  _programID        :: !ByteString,
  _programName      :: !(Maybe ByteString),
  _commandLine      :: !(Maybe Text),
  _prevProgramID    :: !(Maybe ByteString),
  _programDesc      :: !(Maybe Text),
  _programVersion   :: !(Maybe ByteString),
  _programOptFields :: !(Seq RawField)
  } deriving (Default, Generic, Show)

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

makeLenses ''Header

  
