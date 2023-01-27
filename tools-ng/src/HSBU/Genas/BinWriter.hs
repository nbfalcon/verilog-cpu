module HSBU.Genas.BinWriter where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.List.Extra
import Data.Word
import Data.Bits
import Data.Function

hexEncodeW :: Word8 -> String
hexEncodeW w = [ hexC `C8.index` lo, hexC `C8.index` hi ]
    where lo = fromIntegral $ w .&. 0xF
          hi = fromIntegral $ (w .&. 0xF0) `shiftR` 4
          hexC = C8.pack "0123456789ABCDEF"

hexEncodeWordsBE :: BL.ByteString -> C8.ByteString
hexEncodeWordsBE encodeMe = BL.unpack encodeMe & chunksOf 4 & (>>= ((++ "\r\n") . (>>= hexEncodeW))) & C8.pack