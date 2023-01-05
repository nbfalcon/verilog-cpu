module CustomIsaAs where

import Genas
import Data.ByteString.Builder as B
import Data.Word
import Text.Read
import Data.Maybe
import qualified Data.Map as M
import Data.Bits

rTypeCode :: Word32 -> Word32 -> (Word32, Word32, Word32) -> B.Builder
rTypeCode opcode xFunct (rD, rS1, rS2) = word32BE w
    where w = opcode .|. (rD `shiftL` 7) .|. (rS1 `shiftL` 12) .|. (rS2 `shiftL` 17) .|. (xFunct `shiftL` 22) 

iTypeCode :: Word32 -> (Word32, Word32, Word32) -> B.Builder
iTypeCode opcode (rD, rS1, imm15) = word32BE w
    where w = opcode .|. (rD `shiftL` 7) .|. (rS1 `shiftL` 12) .|. (imm15 `shiftL` 17)

jaTypeCode :: Word32 -> (Word32) -> B.Builder
jaTypeCode opcode (dest) = word32BE $ opcode .|. (dest `shiftL` 7)

label1 = fromIntegral `mapExtractor` once exLabelRef

r3 :: AstPosition p => ArgsExtractor p (Word32, Word32, Word32)
r3 = m3to2 $ exRegister `seqTuple` (exRegister `seqTuple` (once exRegister))

r2imm :: AstPosition p => ArgsExtractor p (Word32, Word32, Word32)
r2imm = m3to2 $ exRegister `seqTuple` (exRegister `seqTuple` (once exNum))

parseRegister :: String -> Maybe Int
parseRegister ('r':n) = readMaybe n
parseRegister other = Nothing

ops :: AstPosition p => OpcodesTable p
ops = createOpcodes [ defineOp ["nop"] noargs (\v -> return $ word32LE 0)
                    , defineOp ["add"] r3 (return . rTypeCode 1 0)
                    , defineOp ["sub"] r3 (return . rTypeCode 1 1)
                    , defineOp ["addi"] r2imm (return . iTypeCode 2)
                    , defineOp ["j"] label1 (return . jaTypeCode 3)]

asm :: AstPosition p => Assembler p
asm = Assembler { opcodes=ops, defaultRegister=0, getRegister=parseRegister }
-- nop = defineOp0 ["nop"] (word32LE 0)
-- add = defineOp3 ["add"] r3 (return . rTypeCode 1 0)
-- sub = defineOp3 ["sub"] r3 (return . rTypeCode 1 1)
-- addi = defineOp3 ["addi"] rrn (return . iTypeCode 2)
-- j = defineOp1 ["j"] (label) (return . jaTypeCode 3)

-- asm = Assembler { opcodes = [nop, add, sub, addi, j] }