module HSBU.Genas.Assembler where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Int
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe
import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import Control.Monad

type Labels = M.Map String Int32
labelingPass :: SAST -> Labels
labelingPass = snd . foldl' handleI (0, M.empty)
  where
    handleI (addr, label2addr) (SLabelDecl {labelName}) = (addr, M.insert labelName addr label2addr)
    handleI (addr, label2addr) _anyInstruction = (addr + 4, label2addr)

reduceASTPass :: Labels -> SAST -> LAST
reduceASTPass labels = mapMaybe labelI
  where
    labelI SLabelDecl{} = Nothing
    labelI SInstruction{instructionName, args = sargs, locInstructionName, locArgs} = Just $ LInstruction{instructionId = instructionName, args = reduceArgs sargs, locInstructionName, locArgs}
    reduceArgs = map reduceArg
    reduceArg (SImmediate i) = LImmediate i
    reduceArg (SRegister r) = LRegister r
    reduceArg (SLabelRef name) = LLabelRef $ fromJust $ M.lookup name labels

newtype Assembler' = Assembler' {i2enc :: M.Map String InstructionDef}
getAssembler' :: Assembler -> Assembler'
getAssembler' Assembler{isa} = Assembler'{i2enc = M.fromList $ zip (map name isa) isa}

assemblePass :: Assembler' -> LAST -> AssemblerMonad ByteString
assemblePass Assembler'{i2enc} = fmap (toLazyByteString . mconcat) . mapM encodeI
  where
    encodeI i@LInstruction{instructionId, locInstructionName} = flip mplus (pure mempty) $ case instructionId `M.lookup` i2enc of
      Nothing -> newError locInstructionName ("Unknown instruction " ++ instructionId) >> failAssembly
      Just coder -> do
        encoded <- encoder coder i
        pure $ word32LE encoded

assemble :: Assembler -> SAST -> AssemblerMonad ByteString
assemble asm src = do
    let labels = labelingPass src
    let lAST = reduceASTPass labels src
    let asm' = getAssembler' asm
    assemblePass asm' lAST