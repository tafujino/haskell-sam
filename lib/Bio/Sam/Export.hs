{-# LANGUAGE OverloadedStrings #-} 

module Bio.Sam.Export
  (exportSam
  )
where

import Prelude hiding (seq)
import qualified Bio.BioSeq as BSEQ
import Bio.Sam
import Bio.Sam.Header
import Control.Lens
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import TextShow

star :: Maybe T.Text -> T.Text
star = fromMaybe $ T.singleton '*'

-- !!! this implementation is incomplete !!!
exportHeader :: Header -> T.Text
exportHeader header = T.unlines $ map exportReference $ toList $ header ^. references

-- !!! this implementation is incomplete !!!
exportReference :: Reference -> T.Text
exportReference ref = T.intercalate (T.singleton '\t')
                      [ "@SQ",
                        "SN:" `T.append` (ref ^. refName),
                        "LN:" `T.append` showt (ref ^. refLen) 
                      ]

-- !!! this implementation is incomplete !!!
exportAln :: Aln -> T.Text
exportAln aln = T.intercalate (T.singleton '\t')
                [ aln ^. qname,
                  showt $ aln ^. flag,
                  star $ aln ^. rname,
                  star $ showt . (+ 1) <$> aln ^. pos,
                  star $ showt <$> aln ^. mapq,
                  star $ V.foldl1 T.append . V.map (T.pack . show) . V.convert <$> aln ^. cigars,
                  star $ showt <$> aln ^. rnext,
                  maybe (T.singleton '0') showt (aln ^. pnext),
                  showt $ aln ^. tlen,
                  star $ decodeUtf8 . BSEQ.toIupacByteString <$> aln ^. seq,
                  star $ decodeUtf8 <$> aln ^. qual
                  -- currently opt is discarded
                ]

-- !!! this implementation is incomplete !!!
exportSam :: Sam -> T.Text
exportSam sam = exportHeader (sam ^. header) `T.append` T.unlines (V.toList $ V.map exportAln $ sam ^. alns)
