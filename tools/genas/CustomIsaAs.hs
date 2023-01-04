module CustomIsaAs where

import Genas
import Data.ByteString.Builder as B
import Data.Word
import Data.Maybe
import qualified Data.Map as M
import Data.Bits

reg :: AsmExtractor p Word32
reg (Register ('r':r) p) labels = return $ fromIntegral $ read r
reg x labels = asmErrorL (getPosition x) "Expected a register"

num :: AsmExtractor p Word32
num (Number n p) labels = return $ fromIntegral n
num x labels = asmErrorL (getPosition x) "Expected a number"

label :: AsmExtractor p Word32
label (LabelRef labelName lp) Nothing = return 0
label (LabelRef labelName lp) (Just labels) =
    fromMaybe (asmErrorL lp ("Undefined label: " ++ labelName))
              (Right <$> fromIntegral <$> M.lookup labelName labels)
label x labels = asmErrorL (getPosition x) "Expected a label"

r3 :: (AsmExtractor p1 Word32, AsmExtractor p2 Word32, AsmExtractor p3 Word32)
r3 = (reg,reg,reg)

rrn = (reg,reg,num)

rTypeCode :: Word32 -> Word32 -> (Word32, Word32, Word32) -> B.Builder
rTypeCode opcode xFunct (rD, rS1, rS2) = word32BE w
    where w = opcode .|. (rD `shiftL` 7) .|. (rS1 `shiftL` 12) .|. (rS2 `shiftL` 17) .|. (xFunct `shiftL` 22) 

iTypeCode :: Word32 -> (Word32, Word32, Word32) -> B.Builder
iTypeCode opcode (rD, rS1, imm15) = word32BE w
    where w = opcode .|. (rD `shiftL` 7) .|. (rS1 `shiftL` 12) .|. (imm15 `shiftL` 17)

jaTypeCode :: Word32 -> (Word32) -> B.Builder
jaTypeCode opcode (dest) = word32BE $ opcode .|. (dest `shiftL` 7)

nop = defineOp0 ["nop"] (word32LE 0)
add = defineOp3 ["add"] r3 (return . rTypeCode 1 0)
sub = defineOp3 ["sub"] r3 (return . rTypeCode 1 1)
addi = defineOp3 ["addi"] rrn (return . iTypeCode 2)
j = defineOp1 ["j"] (label) (return . jaTypeCode 3)

asm = Assembler { opcodes = [nop, add, sub, addi, j] }