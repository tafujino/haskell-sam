module Bio.Sam.Convert
  (fromRawSam
  )
where

import Bio.BioSeq
import Bio.BioSeq.DNASeq
import qualified Bio.Sam.RawSam as R
import qualified Bio.Sam as S
import Control.Arrow
import Control.Lens
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Lens
import GHC.Float

fromRawAlnOptValue :: R.AlnOptValue -> S.AlnOptValue
fromRawAlnOptValue (R.AlnOptChar        x) = S.AlnOptChar x
fromRawAlnOptValue (R.AlnOptInt8        x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptUInt8       x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptInt16       x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptUInt16      x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptInt32       x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptUInt32      x) = S.AlnOptInt    $ fromIntegral x
fromRawAlnOptValue (R.AlnOptFloat       x) = S.AlnOptDouble $ float2Double x
fromRawAlnOptValue (R.AlnOptString      x) = S.AlnOptString $ decodeUtf8   x
fromRawAlnOptValue (R.AlnOptByteArray   x) = S.AlnOptByteArray x
fromRawAlnOptValue (R.AlnOptInt8Array   v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptUInt8Array  v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptInt16Array  v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptUInt16Array v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptInt32Array  v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptUInt32Array v) = S.AlnOptIntArray    $ UV.map fromIntegral v
fromRawAlnOptValue (R.AlnOptFloatArray  v) = S.AlnOptDoubleArray $ UV.map float2Double v

fromRawAlnOpt :: R.AlnOpt -> S.AlnOpt
fromRawAlnOpt rawAlnOpt =
  S.AlnOpt { S._alnOptTag   = rawAlnOpt ^. R.alnOptTag,
             S._alnOptValue = rawAlnOpt ^. R.alnOptValue . to fromRawAlnOptValue
           }

fromRawAln :: R.Aln -> S.Aln
fromRawAln rawAln =
  S.Aln { S._qname  = rawAln ^. R.qname,
          S._flag   = rawAln ^. R.flag . to fromIntegral,
          S._rname  = rawAln ^. R.rname,
          S._pos    = rawAln ^. R.pos <&> (subtract 1 >>> fromIntegral),
          S._mapq   = rawAln ^. R.mapq <&> fromIntegral,
          S._cigars = rawAln ^. R.cigars,
          S._rnext  = rawAln ^. R.rnext,
          S._pnext  = rawAln ^. R.pnext <&> fromIntegral,
          S._tlen   = rawAln ^. R.tlen . to fromIntegral,
          S._seq    = rawAln ^. R.seq <&> fromIupacByteString,
          S._qual   = rawAln ^. R.seq,
          S._opt    = rawAln & toVectorOf (R.opt . traverse . to fromRawAlnOpt)
        }

fromRawSam :: R.Sam -> S.Sam
fromRawSam rawSam =
  S.Sam { S._header = rawSam ^. R.header,
          S._alns   = rawSam & toVectorOf (R.alns . traverse . to fromRawAln)
          -- or simply,
          -- S._alns = V.map fromRawAln $ rawSam ^. R.alns
        }
