{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HSBU.Genas.Arch.NBFV3 where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Map qualified as M
import Data.Maybe
import Data.Word (Word32, Word8)
import HSBU.Genas.AssemblerCore
import HSBU.Genas.InstructionParser
import Text.Read (readMaybe)

type Reg = Int

class Args args

-- Using GADT would be less composable some way
data NoArgs = NoArgs deriving (Show, Args)
data R3Args = R3Args {rd :: Reg, rs1 :: Reg, rs2 :: Reg} deriving (Show, Args)
data R2IArgs = R2IArgs {rd :: Reg, rs :: Reg, aluImm :: Int16} deriving (Show, Args)
data JArgs = JArgs {rs1 :: Reg, rs2 :: Reg, jImm :: Int16} deriving (Show, Args)
newtype JAbsArgs = JAbsArgs {jImm :: Int16} deriving stock (Show) deriving anyclass Args
data LSWArgs = LSWArgs {rd :: Reg, rs :: Reg} deriving (Show, Args)

data Instruction args where
  I_NOP :: Instruction NoArgs
  I_ADD :: Instruction R3Args
  I_SUB :: Instruction R3Args
  I_JMP :: Instruction JAbsArgs
  I_ADDI :: Instruction R2IArgs
  I_SUBI :: Instruction R2IArgs
  I_HLT :: Instruction NoArgs
  I_BLT :: Instruction JArgs
  I_BEQ :: Instruction JArgs
  I_BNEQ :: Instruction JArgs
  I_DEBUG_DUMPSTATE :: Instruction NoArgs
  I_LW :: Instruction LSWArgs
  I_LH :: Instruction LSWArgs
  I_LB :: Instruction LSWArgs
  I_SW :: Instruction LSWArgs
  I_SH :: Instruction LSWArgs
  I_SB :: Instruction LSWArgs

type Opcode = Word8
type Funct = Word8

-- GADT is ok here, this is very arch specific anyway
data Coding args where
  -- DuplicateRecordFields does not work for differently-typed GADTs :(
  SimpleCoding :: {_simpleOpcode :: Opcode} -> Coding NoArgs
  JCoding :: {_jOpcode :: Opcode} -> Coding JArgs
  JAbsCoding :: {_jaOpcode :: Opcode} -> Coding JAbsArgs
  R3Coding :: {_r3Opcode :: Opcode, funct :: Funct} -> Coding R3Args
  R2ICoding :: {_r2iOpcode :: Opcode} -> Coding R2IArgs
  LSWCoding :: {_lwOpcode :: Opcode, memMode :: Word8, isStore :: Bool} -> Coding LSWArgs

getCoding :: Instruction args -> Coding args
getCoding I_NOP = SimpleCoding 0
getCoding I_ADD = R3Coding 1 0
getCoding I_SUB = R3Coding 1 1
getCoding I_JMP = JAbsCoding 2
getCoding I_ADDI = R2ICoding 3
getCoding I_SUBI = R2ICoding 4
getCoding I_HLT = SimpleCoding 5
getCoding I_BLT = JCoding 6
getCoding I_BEQ = JCoding 7
getCoding I_BNEQ = JCoding 8
getCoding I_DEBUG_DUMPSTATE = SimpleCoding 9
getCoding I_LW = LSWCoding 10 0 False
getCoding I_LH = LSWCoding 10 1 False
getCoding I_LB = LSWCoding 10 2 False
getCoding I_SW = LSWCoding 10 0 True
getCoding I_SH = LSWCoding 10 1 True
getCoding I_SB = LSWCoding 10 2 True

getFTBitmask :: (Bits a, Num a) => Int -> Int -> a
getFTBitmask fromB toB = complement (complement 0 `shiftL` (toB - fromB + 1))

theseBits :: Integral w => (Int, Int) -> w -> Word32
theseBits (fromB, toB) setTo = (fromIntegral setTo .&. getFTBitmask fromB toB) `shiftL` fromB

exBits :: (Bits a, Num a) => (Int, Int) -> a -> a
exBits (fromB, toB) exFrom = (exFrom `shiftR` fromB) .&. getFTBitmask fromB toB

type CodingHelper = forall w. Integral w => w -> Word32

cOpcode :: CodingHelper
cOpcode = theseBits (0, 5)
cRd :: CodingHelper
cRd = theseBits (6, 10)
cRs1 :: CodingHelper
cRs1 = theseBits (11, 15)
cRs2 :: CodingHelper
cRs2 = theseBits (16, 20)
cFunct :: CodingHelper
cFunct = theseBits (21, 31)
cLSWSel :: CodingHelper
cLSWSel = theseBits (31, 31)
cMemMode :: CodingHelper
cMemMode = theseBits (16, 17)
cJImm :: CodingHelper
cJImm jImm = cRd (exBits (0, 4) i) .|. cFunct (exBits (5, 15) i)
 where
  i :: Word32
  i = fromIntegral jImm
cImm :: CodingHelper
cImm = theseBits (16, 31)

encode :: Coding args -> args -> Word32
encode R3Coding{_r3Opcode, funct} R3Args{rs1, rs2, rd} =
  cOpcode _r3Opcode
    .|. cRd rd
    .|. cRs1 rs1
    .|. cRs2 rs2
    .|. cFunct funct
encode R2ICoding{_r2iOpcode} R2IArgs{rd, rs, aluImm} =
  cOpcode _r2iOpcode
    .|. cRd rd
    .|. cRs1 rs
    .|. cImm aluImm
encode SimpleCoding{_simpleOpcode} _args = fromIntegral _simpleOpcode
encode JCoding{_jOpcode} JArgs{rs1, rs2, jImm} =
  cOpcode _jOpcode
    .|. cRs1 rs1
    .|. cRs2 rs2
    .|. cJImm jImm
encode JAbsCoding{_jaOpcode} JAbsArgs{jImm} =
  cOpcode _jaOpcode .|. cJImm jImm
encode LSWCoding{_lwOpcode, memMode, isStore} LSWArgs{rs, rd} =
  cOpcode _lwOpcode
    .|. cRd rd
    .|. cRs1 rs
    .|. cMemMode memMode
    .|. cLSWSel (if isStore then 1 else 0 :: Int)

regRange :: (Enum b, Show b, Num b) => [Char] -> (b, b) -> b -> [([Char], b)]
regRange nameBase (mapFrom, mapTo) nameFrom = [(nameBase ++ show (nameFrom + i), mapFrom + i) | i <- [0 .. n - 1]]
 where
  n = mapTo - mapFrom + 1

-- Table at: https://en.wikibooks.org/wiki/MIPS_Assembly/Register_File
regTable :: M.Map String Int
regTable =
  M.fromList $
    [("zero", 0), ("r0", 0), ("at", 1)]
      ++ regRange "v" (2, 3) 0
      ++ regRange "a" (4, 7) 0
      ++ regRange "t" (8, 15) 0
      ++ regRange "s" (16, 23) 0
      ++ regRange "t" (24, 25) 8
      ++ regRange "k" (26, 27) 0
      ++ [("gp", 28), ("sp", 29), ("fp", 30), ("ra", 31)]

parseReg :: String -> Int
parseReg regName = fromJust $ readMaybe @Int regName <|> regName `M.lookup` regTable

reg1 :: InstructionArgParser Int
reg1 = parseReg <$> reg

class ParserFor args where
  getParser :: InstructionArgParser args

instance ParserFor NoArgs where
  getParser = pure NoArgs

instance ParserFor R3Args where
  getParser = R3Args <$> reg1 <*> reg1 <*> reg1

instance ParserFor R2IArgs where
  getParser = R2IArgs <$> reg1 <*> reg1 <*> (fromIntegral <$> imm)

instance ParserFor JArgs where
  getParser = JArgs <$> reg1 <*> reg1 <*> (fromIntegral <$> labelRef)

instance ParserFor JAbsArgs where
  getParser = JAbsArgs <$> (fromIntegral <$> labelRef)

instance ParserFor LSWArgs where
  getParser = LSWArgs <$> reg1 <*> reg1

encodeInstruction :: forall args. ParserFor args => Instruction args -> EncodeFunc
encodeInstruction i encodeMe = do
  args' <- runGetArgs (getParser @args) encodeMe
  let coding = getCoding i
  return $ encode coding args'

defInstruction :: ParserFor args => String -> Instruction args -> InstructionDef
defInstruction name i = InstructionDef{name = name, encoder = encodeInstruction i}

assembler :: Assembler
assembler =
  Assembler
    { isa =
        [ defInstruction "nop" I_NOP
        , defInstruction "add" I_ADD
        , defInstruction "sub" I_SUB
        , defInstruction "jmp" I_JMP
        , defInstruction "addi" I_ADDI
        , defInstruction "subi" I_SUBI
        , defInstruction "hlt" I_HLT
        , defInstruction "blt" I_BLT
        , defInstruction "beq" I_BEQ
        , defInstruction "bneq" I_BNEQ
        , defInstruction "debugDumpState" I_DEBUG_DUMPSTATE
        , defInstruction "lw" I_LW
        , defInstruction "lh" I_LH
        , defInstruction "lb" I_LB
        , defInstruction "sw" I_SW
        , defInstruction "sh" I_SH
        , defInstruction "sb" I_SB
        ]
    }