module Encode where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import Data.Word
import Data.Bits

chunkList :: Int -> [a] -> [[a]]
chunkList n = chunk1 n
  where chunk1 thisChunk [] = []
        chunk1 0 rest = []:(chunk1 n rest)
        chunk1 thisChunk [single] = [[single]]
        chunk1 thisChunk (fst:rest) = let (curChunk:restChunks) = chunk1 (thisChunk - 1) rest in (fst:curChunk):restChunks

hexEncodeW8 :: Word8 -> (Char, Char)
hexEncodeW8 w = (hexS !! h, hexS !! l)
    where l = fromIntegral $ w .&. 0xF
          h = fromIntegral $ (w .&. 0xF0) `shiftR` 4
          hexS = "0123456789ABCDEF"

hexEncodeW8B :: Word8 -> B.Builder
hexEncodeW8B w = B.char7 h `mappend` B.char7 l
    where (h, l) = hexEncodeW8 w

crLf = B.char7 '\r' `mappend` B.char7 '\n'

-- hexEncode :: ByteString -> ByteString
hexEncodeG s = mconcat $ map ((`mappend` crLf) . (mconcat . map hexEncodeW8B)) $ chunkList 4 $ L.unpack s