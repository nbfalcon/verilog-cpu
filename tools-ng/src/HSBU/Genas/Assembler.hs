module HSBU.Genas.Assembler where

import Control.Monad
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe
import Data.Semigroup
import Data.Text.Encoding qualified as T
import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import qualified Data.Text as T

type Labels = M.Map String Int32
labelingPass :: SAST -> Labels
labelingPass = snd . foldl' handleI (0, M.empty)
 where
  handleI :: (Int32, Labels) -> SLine -> (Int32, Labels)
  handleI (addr, label2addr) SLabelDecl{labelName} = (addr, M.insert labelName addr label2addr)
  handleI (addr, label2addr) (SOpInjection OStringInCode{text}) = (addr + fromIntegral (T.length text), label2addr)
  handleI (addr, label2addr) (SOpInjection OAlignDecl{alignTo}) = (addr + (alignTo - addr) `mod` alignTo, label2addr)
  handleI (addr, label2addr) SInstruction{} = (addr + 4, label2addr)

reduceASTPass :: Labels -> SAST -> LAST
reduceASTPass labels = mapMaybe labelI
 where
  labelI SLabelDecl{} = Nothing
  labelI SInstruction{instructionName, args = sargs, locInstructionName, locArgs} = Just $ LInstruction{instructionId = instructionName, args = reduceArgs sargs, locInstructionName, locArgs}
  labelI (SOpInjection w) = pure $ LOpInjection w
  reduceArgs = map reduceArg
  reduceArg (SImmediate i) = LImmediate i
  reduceArg (SRegister r) = LRegister r
  reduceArg (SLabelRef name) = LLabelRef $ fromJust $ M.lookup name labels

newtype Assembler' = Assembler' {i2enc :: M.Map String InstructionDef}
getAssembler' :: Assembler -> Assembler'
getAssembler' Assembler{isa} = Assembler'{i2enc = M.fromList $ zip (map name isa) isa}

assemblePass :: Assembler' -> LAST -> AssemblerMonad BL.ByteString
assemblePass Assembler'{i2enc} = fmap (toLazyByteString . mconcat) . mapM (encodeI 0)
 where
  encodeI _ip LInstruction{instructionId, args, locInstructionName, locArgs} = flip mplus (pure mempty) $ case instructionId `M.lookup` i2enc of
    Nothing -> newError locInstructionName ("Unknown instruction " ++ instructionId) >> failAssembly
    Just coder -> do
      encoded <- encoder coder $ EncodeMe{instructionId, args, locInstructionName, locArgs}
      pure $ word32LE encoded
  encodeI _ip (LOpInjection (OStringInCode{text})) = pure $ T.encodeUtf8Builder text
  encodeI ip (LOpInjection (OAlignDecl{alignTo})) = pure $ stimes ((alignTo - ip) `mod` alignTo) $ word8 0

assemble :: Assembler -> SAST -> AssemblerMonad BL.ByteString
assemble asm src = do
  let labels = labelingPass src
  let lAST = reduceASTPass labels src
  let asm' = getAssembler' asm
  assemblePass asm' lAST