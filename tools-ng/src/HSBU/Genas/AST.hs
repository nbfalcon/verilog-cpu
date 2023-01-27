{-# LANGUAGE DuplicateRecordFields #-}

module HSBU.Genas.AST where
import Data.Int

data SLocation = SLocation { sourceLine :: !Int, sourceColumn :: !Int, sourceFileName :: !FilePath }

instance Show SLocation where
    show SLocation { sourceLine, sourceColumn, sourceFileName } = sourceFileName ++ ":" ++ show sourceLine ++ ":" ++ show sourceColumn

-- S for source: this is the first high-level AST
data SArg = SImmediate Integer | SRegister String | SLabelRef String deriving (Show)
data SLine
    = SInstruction {instructionName :: String, args :: [SArg], locInstructionName :: SLocation, locArgs :: [SLocation]}
    | SLabelDecl {labelName :: String, locLabel :: SLocation} deriving (Show)
type SAST = [SLine]

-- L for low-level: this is the second low-level AST, which can be assembled directly
data LArg
    = LImmediate Integer
    | LRegister String
    | LLabelRef Int32
data LLine = LInstruction {instructionId :: String, args :: [LArg], locInstructionName :: SLocation, locArgs :: [SLocation]}
type LAST = [LLine]