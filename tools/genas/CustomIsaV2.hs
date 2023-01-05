module CustomIsaV2 where

import Data.Word
import Data.Bits
import Text.Read
import qualified Data.ByteString.Builder as B
import Genas

rTypeCode :: Word32 -> Word32 -> (Word32, Word32, Word32) -> B.Builder
rTypeCode opcode xFunct (rD, rS1, rS2) = B.word32BE w
    where w = opcode .|. (rD `shiftL` 6) .|. (rS1 `shiftL` 11) .|. (rS2 `shiftL` 16) .|. (xFunct `shiftL` 21) 

iTypeCode :: Word32 -> (Word32, Word32, Word32) -> B.Builder
iTypeCode opcode (rd, rs1, imm16) = B.word32BE w
    where w = opcode .|. (rd `shiftL` 6) .|. (rs1 `shiftL` 11) .|. (imm16 `shiftL` 16)

jaTypeCode :: Word32 -> Word32 -> B.Builder
jaTypeCode opcode jmpTo = iTypeCode opcode (0, 0, jmpTo)

noArgsCode :: Word32 -> () -> B.Builder
noArgsCode opcode nothing = B.word32BE opcode

r3 :: AstPosition p => ArgsExtractor p (Word32, Word32, Word32)
r3 = m3to2 $ exRegister `seqTuple` (exRegister `seqTuple` (once exRegister))

r2imm :: AstPosition p => ArgsExtractor p (Word32, Word32, Word32)
r2imm = m3to2 $ exRegister `seqTuple` (exRegister `seqTuple` (once exNum))

label1 :: ArgsExtractor p Word32
label1 = fromIntegral `mapExtractor` once exLabelRef

parseRegister :: String -> Maybe Int
parseRegister ('r':n) = readMaybe n
parseRegister other = Nothing

ops :: AstPosition p => OpcodesTable p
ops = createOpcodes [ defineOp ["nop"] noargs (return . noArgsCode 0)
                    , defineOp ["add"] r3 (return . rTypeCode 1 0)
                    , defineOp ["sub"] r3 (return . rTypeCode 1 1)
                    , defineOp ["j"] label1 (return . jaTypeCode 2)
                    , defineOp ["addi"] r2imm (return . iTypeCode 3)
                    , defineOp ["subi"] r2imm (return . iTypeCode 4)
                    , defineOp ["hlt"] noargs (return . noArgsCode 5)
                    , defineOp ["blt"] r2imm (return . iTypeCode 6)
                    , defineOp ["beq"] r2imm (return . iTypeCode 7)
                    , defineOp ["bneq"] r2imm (return . iTypeCode 8)
                    , defineOp ["debugDumpState"] noargs (return . noArgsCode 9)
                    ]

asm :: AstPosition p => Assembler p
asm = Assembler { opcodes=ops, defaultRegister=0, getRegister=parseRegister }
