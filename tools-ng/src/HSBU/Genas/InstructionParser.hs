{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module HSBU.Genas.InstructionParser where

import Data.Bifunctor
import Data.Either.Combinators
import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import Data.Int
import Data.ByteString (StrictByteString)

type OneArgParser a = LArg -> AssemblerMonad a
type ArgParserFunc a = [LArg] -> AssemblerMonad ([LArg], a)

newtype InstructionArgParser a = InstructionArgParser {runInstructionArgParser :: ArgParserFunc a}

runGetArgs :: InstructionArgParser a -> [LArg] -> AssemblerMonad a
runGetArgs InstructionArgParser {runInstructionArgParser} = (snd <$>) . runInstructionArgParser

instance Functor InstructionArgParser where
    f `fmap` InstructionArgParser{runInstructionArgParser} = InstructionArgParser $ \args -> (second f) <$> runInstructionArgParser args

instance Applicative InstructionArgParser where
    pure v = InstructionArgParser $ pure . (,v)
    InstructionArgParser{runInstructionArgParser = l} <*> InstructionArgParser{runInstructionArgParser = r} =
        InstructionArgParser $ \args -> do
            (rest, mapper) <- l args
            (rest', v) <- r rest
            return (rest', mapper v)

imm :: OneArgParser Integer
imm (LImmediate i) = pure i
imm _ = errorV "Expected immeidate" 0

reg :: OneArgParser String
reg (LRegister r) = pure r
reg _ = errorV "Expected register" "Meow"

labelRef :: OneArgParser Int32
labelRef (LLabelRef address) = pure address
labelRef _ = errorV "Expected address" 0

oneArg' :: OneArgParser a -> ArgParserFunc a
oneArg' f [] = errorV "Expected an argument" undefined
oneArg' f (a : xs) = do
    -- This could be rewritten with cute applicatives and tuple sections, but this is cleaner :)
    r <- f a
    return (xs, r)

noMore' [] = pure ([], ())
noMore' (x:xs) = errorV "Expected no more arguments" ([], ()) 

oneArg :: OneArgParser a -> InstructionArgParser a
oneArg f = InstructionArgParser $ oneArg' f

noMore = InstructionArgParser noMore'