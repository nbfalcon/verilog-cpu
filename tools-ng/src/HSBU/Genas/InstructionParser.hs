{-# LANGUAGE NamedFieldPuns #-}

module HSBU.Genas.InstructionParser where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer hiding (tell)
import Data.Int
import Data.Maybe (isNothing)
import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import Control.Monad.RWS (MonadWriter)
import Control.Monad.RWS.Class (MonadWriter(..))
import Data.List (singleton)

-- data InstructionArgParserError = InstructionArgParserError { message :: String, locationArgIndex :: !Int }
-- data InstructionArgParserState = InstructionArgParserState { remainingArgs :: [LArg], locationArgIndex :: !Int }
-- newtype InstructionArgParserMonad a = InstructionArgParserMonad { runInstructionArgParserM :: Writer [InstructionArgParserError] a } deriving (Functor, Applicative, Monad)

data InstructionArgParserError = ArgParserE {message :: String, atArgIndex :: Int}
data InstructionArgParserState = InstructionArgParserState {curArgIndex :: !Int, remainingArgs :: [LArg]}
newtype InstructionArgParserM a = InstructionArgParserM {runInstructionArgParserM :: MaybeT (StateT InstructionArgParserState (Writer [InstructionArgParserError])) a} deriving (Functor, Applicative, Monad, Alternative)
deriving instance MonadWriter [InstructionArgParserError] InstructionArgParserM
deriving instance MonadState InstructionArgParserState InstructionArgParserM

unconsMay :: [a] -> Maybe (a, [a])
unconsMay [] = Nothing
unconsMay (x : xs) = Just (x, xs)

liftMaybe :: Maybe a -> InstructionArgParserM a
liftMaybe = InstructionArgParserM . MaybeT . pure

nextArg :: InstructionArgParserM LArg
nextArg = do
    InstructionArgParserState{curArgIndex, remainingArgs} <- get
    (hd, tl) <- liftMaybe $ unconsMay remainingArgs
    put InstructionArgParserState{curArgIndex = curArgIndex + 1, remainingArgs = tl}
    pure hd

instructionParserError :: String -> InstructionArgParserM ()
instructionParserError message = do
    InstructionArgParserState{curArgIndex} <- get
    tell $ singleton ArgParserE{message=message, atArgIndex=curArgIndex}

failedDefaultValue :: a -> InstructionArgParserM a
failedDefaultValue = pure

-- Fail: we didn't get the right combination of arguments
failInstructionParser :: InstructionArgParserM a
failInstructionParser = liftMaybe Nothing

data InstructionArgParser a = InstructionArgParser {runInstructionArgParser1 :: InstructionArgParserM a, expects :: [String]}

formatExpects :: [String] -> String
formatExpects [] = "<No arguments>"
formatExpects expected = unwords $ map (\s -> "<" ++ s ++ ">") expected

getTypeS :: LArg -> String
getTypeS (LRegister _) = "reg"
getTypeS (LImmediate _) = "imm"
getTypeS (LLabelRef _) = "label"

runInstructionArgParser :: [LArg] -> InstructionArgParser a -> (Maybe a, InstructionArgParserState, [InstructionArgParserError])
runInstructionArgParser args = (\((r, s), e) -> (r, s, e)) . runWriter . flip runStateT InstructionArgParserState{curArgIndex = 0, remainingArgs = args} . runMaybeT . runInstructionArgParserM . runInstructionArgParser1

runGetArgs :: InstructionArgParser a -> EncodeMe -> AssemblerMonad a
runGetArgs p@InstructionArgParser{expects} EncodeMe{args, locInstructionName, locArgs} = do
    when haveWrongArguments wrongArgumentsError
    forM_ bonusErrors $ \case
        ArgParserE{atArgIndex, message} -> newError (locArgs !! atArgIndex) message
    maybe failAssembly pure result
  where
    (result, InstructionArgParserState{remainingArgs}, bonusErrors) = runInstructionArgParser args p
    haveWrongArguments = isNothing result || not (null remainingArgs)
    wrongArgumentsError = newError locInstructionName ("Expected " ++ expectedMsg ++ ", but got: " ++ actualMsg)
    expectedMsg = formatExpects expects
    actualMsg = formatExpects $ map getTypeS args

instance Functor InstructionArgParser where
    f `fmap` InstructionArgParser{runInstructionArgParser1, expects} = InstructionArgParser{runInstructionArgParser1 = f <$> runInstructionArgParser1, expects}

instance Applicative InstructionArgParser where
    pure v = InstructionArgParser{runInstructionArgParser1 = pure v, expects = []}
    InstructionArgParser{runInstructionArgParser1 = l, expects = le} <*> InstructionArgParser{runInstructionArgParser1 = r, expects = re} =
        InstructionArgParser{runInstructionArgParser1 = l <*> r, expects = le ++ re}

composeParser :: InstructionArgParser a -> (a -> InstructionArgParserM b) -> InstructionArgParser b
composeParser InstructionArgParser{runInstructionArgParser1, expects} flatMapper
    = InstructionArgParser{runInstructionArgParser1=runInstructionArgParser1 >>= flatMapper, expects}

oneArg' :: String -> (LArg -> InstructionArgParserM a) -> InstructionArgParser a
oneArg' expect subParser = InstructionArgParser{runInstructionArgParser1 = nextArg >>= subParser, expects = [expect]}

imm :: InstructionArgParser Integer
imm = oneArg' "imm" $ \case
    LImmediate i -> pure i
    LLabelRef a -> pure (fromIntegral a)
    _ -> failInstructionParser

reg :: InstructionArgParser String
reg = oneArg' "reg" $ \case
    LRegister r -> pure r
    _ -> failInstructionParser

labelRef :: InstructionArgParser Int32
labelRef = oneArg' "label" $ \case
    LLabelRef address -> pure address
    _ -> failInstructionParser