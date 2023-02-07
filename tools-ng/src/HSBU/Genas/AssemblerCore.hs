{-# LANGUAGE TypeFamilyDependencies #-}

module HSBU.Genas.AssemblerCore where

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Word
import HSBU.Genas.AST
import Data.List (singleton)
import Control.Applicative (Alternative)

data AssemblerError = ErrorMessage {message :: String, location :: !SLocation}

newtype AssemblerMonad a = AssemblerMonad {runAssemblerM :: MaybeT (Writer [AssemblerError]) a} deriving (Functor, Applicative, Monad, Alternative, MonadPlus)
deriving instance MonadWriter [AssemblerError] AssemblerMonad

instance Show AssemblerError where
    show ErrorMessage{message, location} = show location ++ ": " ++ message

liftErrors :: [AssemblerError] -> AssemblerMonad ()
liftErrors = AssemblerMonad . tell

newError :: SLocation -> String -> AssemblerMonad ()
newError location message = AssemblerMonad $ tell $ singleton $ ErrorMessage{location, message}

failAssembly :: AssemblerMonad a
failAssembly = mzero

type EncodeFunc = EncodeMe -> AssemblerMonad Word32
data InstructionDef = InstructionDef {name :: String, encoder :: EncodeFunc}

data Assembler = Assembler {isa :: [InstructionDef]}

runAssembler :: AssemblerMonad a -> (Maybe a, [AssemblerError])
runAssembler = runWriter . runMaybeT . runAssemblerM