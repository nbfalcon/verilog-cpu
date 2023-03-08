{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module HSBU.Genas.AST where

import Data.Int
import Data.Text qualified as T
import Data.List (intercalate)

data SLocation = SLocation {sourceLine :: !Int, sourceColumn :: !Int, sourceFileName :: !FilePath} | GeneratedCode

-- S for source: this is the first high-level AST
data SWrappedArg = SWrappedString {text :: T.Text}
data SArg
    = SImmediate Integer
    | SRegister String
    | SLabelRef String
    | SPseudoLabelRef Int
    | SWrapInLabel SWrappedArg

data OpInjection
    = OStringInCode {text :: T.Text, locText :: SLocation}
    | OAlignDecl {alignTo :: Int32, locAlign :: SLocation}
data SLine
    = SInstruction {instructionName :: String, args :: [SArg], locInstructionName :: SLocation, locArgs :: [SLocation]}
    | SLabelDecl {labelName :: String, locLabel :: SLocation}
    | SWrapLabelUseSection {useWrapSection :: T.Text, locUseWrapSection :: SLocation}
    | SWrapLabelSection {declWrapSection :: T.Text, locDeclWrapSection :: SLocation}
    | SPseudoLabelDecl {labelId :: Int} -- No location; this is generated code
    | SOpInjection OpInjection
type SAST = [SLine]

instance Show SWrappedArg where
    show SWrappedString{text} = "&" ++ show text
instance Show SLocation where
    show SLocation{sourceLine, sourceColumn, sourceFileName} = sourceFileName ++ ":" ++ show sourceLine ++ ":" ++ show sourceColumn
    show GeneratedCode = "<Generated>"
instance Show OpInjection where
    show OStringInCode{text} = ".ascii " ++ show text
    show OAlignDecl{alignTo} = ".align " ++ show alignTo
instance Show SArg where
    show (SImmediate i) = show i
    show (SRegister r) = "$" ++ r
    show (SLabelRef r) = ":" ++ r
    show (SPseudoLabelRef i) = ":" ++ show i
    show (SWrapInLabel w) = show w
instance Show SLine where
    show SInstruction{instructionName, args=[]} = instructionName
    show SInstruction{instructionName, args} = instructionName ++ " " ++ intercalate ", " (show <$> args)
    show SLabelDecl{labelName} = labelName ++ ":"
    show SWrapLabelUseSection{useWrapSection} = ".use " ++ T.unpack useWrapSection
    show SWrapLabelSection{declWrapSection} = ".wrapHere " ++ T.unpack declWrapSection
    show SPseudoLabelDecl{labelId} = show labelId ++ ":"
    show (SOpInjection i) = show i

-- L for low-level: this is the second low-level AST, which can be assembled directly
data LArg
    = LImmediate Integer
    | LRegister String
    | LLabelRef Int32
    deriving (Show)
data LLine
    = LInstruction {instructionId :: String, args :: [LArg], locInstructionName :: SLocation, locArgs :: [SLocation]}
    | LOpInjection OpInjection
    deriving (Show)
type LAST = [LLine]

data EncodeMe = EncodeMe {instructionId :: String, args :: [LArg], locInstructionName :: SLocation, locArgs :: [SLocation], instructionPointer :: Int32}