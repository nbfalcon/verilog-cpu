{-# LANGUAGE DuplicateRecordFields #-}

module HSBU.Genas.AST where
import Data.Int

-- S for source: this is the first high-level AST
data SArg = SImmediate Integer | SRegister String | SLabelRef String deriving (Show)
data SLine
    = SInstruction {instructionName :: String, args :: [SArg]}
    | SLabelDecl {labelName :: String} deriving (Show)
type SAST = [SLine]

-- L for low-level: this is the second low-level AST, which can be assembled directly
data LArg
    = LImmediate Integer
    | LRegister String
    | LLabelRef Int32
data LLine = LInstruction {instructionId :: String, args :: [LArg]}
type LAST = [LLine]