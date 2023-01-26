{-# LANGUAGE TypeFamilyDependencies #-}

module HSBU.Genas.AssemblerCore where

import Control.Monad.Trans.Writer
import Data.Word
import HSBU.Genas.AST
import Data.List (singleton)

newtype AssemblerError = ErrorMessage String
newtype AssemblerMonad a = AssemblerMonad {runAssemblerM :: Writer [AssemblerError] a} deriving (Functor, Applicative, Monad)

instance Show AssemblerError where
    show (ErrorMessage s) = s

newError :: String -> AssemblerMonad ()
newError = AssemblerMonad . tell . singleton . ErrorMessage

errorV :: String -> a -> AssemblerMonad a
errorV m defaultValue = newError m >> pure defaultValue

data EncodeMe = EncodeMe {instructionId :: String, args :: [LArg]}

type EncodeFunc = EncodeMe -> AssemblerMonad Word32
data InstructionDef = InstructionDef {name :: String, encoder :: EncodeFunc}

data Assembler = Assembler {isa :: [InstructionDef]}

runAssembler :: AssemblerMonad a -> (a, [AssemblerError])
runAssembler = runWriter . runAssemblerM