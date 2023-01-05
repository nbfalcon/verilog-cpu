{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Genas where

import Data.List
import Text.Printf
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Builder as B
import Data.Maybe
import ErrorCollectorM

data AstId p = AstId { name :: String, position :: p }
data AstArg p =
    Number { value :: Integer, position :: p }
    | Register { registerName :: String, position :: p }
    | LabelRef { labelRef :: String, position :: p }
data AstLine p =
    Label { labelName :: String, position :: p }
    | Op { opName :: (AstId p), opArgs :: [AstArg p], position :: p }
data AstTU p = AstTU { lines :: [AstLine p], position :: p }

class WithPosition n where
    getPosition :: n p -> p
instance WithPosition AstId where
    getPosition AstId { position } = position
instance WithPosition AstArg where
    getPosition Number { position } = position
    getPosition Register { position } = position
    getPosition LabelRef { position } = position
instance WithPosition AstLine where
    getPosition Label { position } = position
    getPosition Op { position } = position
instance WithPosition AstTU where
    getPosition AstTU { position } = position

instance Show (AstId p) where
    show AstId { name } = name
instance Show (AstArg p) where
    show Number { value } = "$" ++ show value
    show Register { registerName } = registerName
    show LabelRef { labelRef } = ":" ++ labelRef
instance Show (AstLine p) where
    show Label { labelName } = labelName ++ ":"
    show Op { opName=AstId { name }, opArgs=[] } = name
    show Op { opName=AstId { name }, opArgs=args } = name ++ " " ++ intercalate ", " (map show args)
instance Show (AstTU p) where
    show AstTU { lines } = concat $ map ((++ "\n") . show) lines

type CapturedPrintf = (String, String -> String)

createError pos s printer = newError $ CustomMessage { message = (s, printer), position = pos }

data ArgType = TLabelRef | TNumber | TRegister
data AsmError p = CustomMessage { message :: CapturedPrintf, position :: p }

instance Show p => Show (AsmError p) where
    show CustomMessage { message=(m, p), position } = show position ++ ": " ++ (p m)

data AsmArg p =
    AsmNumber { value :: Integer, position :: p }
    | AsmRegister { code :: Int, position :: p }
    | AsmLabel { address :: Int, position :: p }
    | NothingArg
data AsmLine p = AsmOp { name :: String, args :: [AsmArg p], position :: p }

instance WithPosition AsmArg where
    getPosition AsmNumber { position } = position
    getPosition AsmRegister { position } = position
    getPosition AsmLabel { position } = position

type AsmMonad p r = ErrorCollectorM (AsmError p) r

type ArgExtractor p a = AsmArg p -> AsmMonad p a
type ArgsExtractor p a = [AsmArg p] -> AsmMonad p a
type AsmFunc p a = a -> AsmMonad p B.Builder
type AsmOpFunc p = AsmFunc p [AsmArg p]

exNum :: ArgExtractor p Integer
exNum AsmNumber { value } = return value
exNum other = 0 <$ createError (getPosition other) "Expected a number" id

exRegister AsmRegister { code } = return code
exRegister other = 0 <$ createError (getPosition other) "Expected a register" id

exLabelRef AsmLabel { address } = return address
exLabelRef other = 0 <$ createError (getPosition other) "Expected a label" id

once :: ArgExtractor p a -> ArgsExtractor p a
once ex [e] = ex e
once ex [] = ex NothingArg -- FIXME error here
once ex (e:sndA:es) = do createError (getPosition sndA) "Excess arguments" id; ex e

noargs :: ArgsExtractor p ()
noargs [] = return ()
noargs (e:rest) = createError (getPosition e) "No arguments expected" id

seqTuple :: ArgExtractor p a -> ArgsExtractor p b -> ArgsExtractor p (a, b)
-- FIXME: error here
-- seqTuple hd tl [] = do createError (getPosition sndA) "Excess arguments" id; hd NothingArg
seqTuple hd tl (fst:xs) = do
    first <- hd fst
    rest <- tl xs
    return $ (first, rest)

mapExtractor :: (a -> b) -> ArgsExtractor p a -> ArgsExtractor p b
mapExtractor mapper ex args = do r <- ex args; return $ mapper r

data Opcode p = Opcode { names :: [String], encode :: AsmOpFunc p }

defineOp :: [String] -> ArgsExtractor p a -> AsmFunc p a -> Opcode p
defineOp names ex asm = Opcode { names, encode=asmF }
    where asmF args = do extracted <- ex args; asm extracted

data Assembler p = Assembler { opcodes :: M.Map String (Opcode p)
                             , getRegister :: String -> Maybe Int
                             , defaultRegister :: Int
                             }

createOpcodes :: [Opcode p] -> M.Map String (Opcode p)
createOpcodes ops = M.fromList $ do
    op@Opcode { names } <- ops
    names `zip` (repeat op)

data LabelXref p = LabelXref { address :: Int, position :: p }

type Labels p = M.Map String (LabelXref p)

labelPass :: Assembler p -> AstTU p -> AsmMonad p (Labels p)
labelPass asm AstTU { lines } = snd <$> foldM doLabel1 (0, M.empty) lines
    where doLabel1 (adr, label2Adr) Label { labelName, position=labPos } =
            case M.lookup labelName label2Adr of
                Just LabelXref { position=oldPos } -> do
                    -- FIXME: somehow print oldPos; this will require more constraints on p
                    createError labPos "Duplicate label definition '%s' (alreay defined at '')" (`printf` labelName)
                    return (adr, label2Adr) 
                Nothing -> return (adr, M.insert labelName LabelXref { address=adr, position=labPos } label2Adr)
          doLabel1 (adr, label2Adr) Op { opName, opArgs } = return (adr + 4, label2Adr) -- FIXME: don't hardcode 4

reduce2AsmPass :: Assembler p -> Labels p -> AstTU p -> AsmMonad p [AsmLine p]
reduce2AsmPass asm labels AstTU { lines } = mapMaybe id <$> mapM mapLine lines
    where -- We can't use real type declarations here, because we can't reuse the outer 'p' internally
        -- mapLine :: AstLine p -> AsmMonad p (Maybe (AsmLine p))
          mapLine Label { labelName, position=labPos } = return Nothing
          mapLine Op { opName=AstId { name=opName }, opArgs, position } = do
            args <- mapM mapOp opArgs
            return $ Just AsmOp { name=opName, args, position }
            
        --   mapOp :: AstArg p -> AsmMonad p (AsmArg p)
          mapOp Number { value, position } = return AsmNumber { value, position }
          mapOp Register { registerName, position=rp } = case getRegister asm registerName of
            Just r -> return AsmRegister { code=r, position=rp }
            Nothing -> do
                createError rp "Unknown register '%s'" (`printf` registerName)
                return AsmRegister { code=(defaultRegister asm), position=rp }
          mapOp LabelRef { labelRef, position=lp } = case M.lookup labelRef labels of
            Just LabelXref { address=adr, position=xp } -> return AsmLabel { address=adr, position=xp }
            Nothing -> do
                createError lp "Undefined label '%s'" (`printf` labelRef)
                return AsmLabel { address=0, position=lp }

assemble1 :: Assembler p -> AsmLine p -> AsmMonad p B.Builder
assemble1 Assembler { opcodes } AsmOp { name, args, position } = case M.lookup name opcodes of
    Just Opcode { encode } -> encode args
    Nothing -> do
        createError position "Unknown operation '%s'" (`printf` name)
        return mempty

assemblePass :: Assembler p -> [AsmLine p] -> AsmMonad p B.Builder
assemblePass asm lines = mconcat <$> mapM (assemble1 asm) lines

assemble :: Assembler p -> AstTU p -> AsmMonad p B.Builder
assemble asm tu = do
    labels <- labelPass asm tu
    simpleOps <- reduce2AsmPass asm labels tu
    result <- assemblePass asm simpleOps
    return $ result