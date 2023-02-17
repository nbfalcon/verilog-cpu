module HSBU.Genas.BinWriter where

import Control.Monad
import Data.Bits
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Function
import Data.List.Extra
import Data.Word

hexEncodeW :: Word8 -> String
hexEncodeW w = [hexC `C8.index` hi, hexC `C8.index` lo]
  where
    lo = fromIntegral $ w .&. 0xF
    hi = fromIntegral $ (w .&. 0xF0) `shiftR` 4
    hexC = C8.pack "0123456789ABCDEF"

hexEncodeWordsBE :: BL.ByteString -> C8.ByteString
hexEncodeWordsBE encodeMe = BL.unpack encodeMe & chunksOf 4 & (>>= ((++ "\r\n") . (hexEncodeW <=< reverse))) & C8.pack