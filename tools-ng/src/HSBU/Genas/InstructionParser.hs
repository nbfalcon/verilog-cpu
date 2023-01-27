{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module HSBU.Genas.InstructionParser where

import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import Data.Int
import Control.Monad

-- data InstructionArgParserError = InstructionArgParserError { message :: String, locationArgIndex :: !Int }
-- data InstructionArgParserState = InstructionArgParserState { remainingArgs :: [LArg], locationArgIndex :: !Int }
-- newtype InstructionArgParserMonad a = InstructionArgParserMonad { runInstructionArgParserM :: Writer [InstructionArgParserError] a } deriving (Functor, Applicative, Monad)

data InstructionArgParser a = InstructionArgParser {runInstructionArgParser :: [LArg] -> Maybe (a, [LArg]), expects :: [String] }

formatExpects :: [String] -> String
formatExpects [] = "<No arguments>"
formatExpects expected = unwords $ map (\s -> "<" ++ s ++ ">") expected

getTypeS :: LArg -> String
getTypeS (LRegister _) = "reg"
getTypeS (LImmediate _) = "imm"
getTypeS (LLabelRef _) = "label"

runGetArgs :: InstructionArgParser a -> LLine -> AssemblerMonad a
runGetArgs InstructionArgParser {runInstructionArgParser, expects} LInstruction { args, locInstructionName } = case runInstructionArgParser args >>= consumedAll of
    Nothing -> newError locInstructionName ("Expected " ++ expectedMsg ++ ", but got: " ++ actualMsg) >> mzero
    Just v -> pure v
    where consumedAll (v, []) = Just v
          consumedAll (_v, _x:_xs) = Nothing
          expectedMsg = formatExpects expects
          actualMsg = formatExpects $ map getTypeS args

instance Functor InstructionArgParser where
    f `fmap` InstructionArgParser{runInstructionArgParser, expects} = InstructionArgParser {runInstructionArgParser=run, expects}
        where run args = do
                (v, rest) <- runInstructionArgParser args
                pure (f v, rest)

instance Applicative InstructionArgParser where
    pure v = InstructionArgParser {runInstructionArgParser = Just . (v,), expects=[]}
    InstructionArgParser{runInstructionArgParser = l, expects=le} <*> InstructionArgParser{runInstructionArgParser = r, expects=re} =
        InstructionArgParser { runInstructionArgParser=run, expects=le ++ re }
        where run args = do
                (mapper, rest) <- l args
                (v, rest') <- r rest
                return (mapper v, rest')

oneArg' :: String -> (LArg -> Maybe a) -> InstructionArgParser a
oneArg' expect subParser = InstructionArgParser {runInstructionArgParser=run, expects=[expect]}
    where run = \case
            [] -> Nothing
            (arg:rest) -> (, rest) <$> subParser arg

imm :: InstructionArgParser Integer
imm = oneArg' "imm" $ \case
    LImmediate i -> pure i
    _ -> Nothing

reg :: InstructionArgParser String
reg = oneArg' "reg" $ \case
    LRegister r -> pure r
    _ -> Nothing

labelRef :: InstructionArgParser Int32
labelRef = oneArg' "label" $ \case
    LLabelRef address -> pure address
    _ -> Nothing