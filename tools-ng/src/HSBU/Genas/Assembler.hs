{-# LANGUAGE UndecidableInstances #-}

module HSBU.Genas.Assembler where

import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.State
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.IntMap.Strict qualified as MI
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Semigroup
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import HSBU.Genas.AST
import HSBU.Genas.AssemblerCore
import qualified Data.ByteString as BS

data UnwrapPassState = UnwrapPassState {curUseDecl :: T.Text, nextPseudoLabel :: !Int, use2Ast :: M.Map T.Text SAST}
defaultUnwrapState :: UnwrapPassState
defaultUnwrapState = UnwrapPassState{curUseDecl = T.pack "", nextPseudoLabel = 0, use2Ast = M.empty}
unwrapWrapArgs :: SAST -> (SAST, M.Map T.Text SAST)
unwrapWrapArgs ast = (ast', patches)
 where
  (ast', UnwrapPassState{use2Ast = patches}) = runState (mapMaybeM reduceWrapI ast) defaultUnwrapState
  reduceWrapArg1 :: SWrappedArg -> State UnwrapPassState SArg
  reduceWrapArg1 SWrappedString{text} = do
    curUseDecl <- gets curUseDecl
    pseudoLabel <- gets nextPseudoLabel
    modify' (\s@UnwrapPassState{nextPseudoLabel} -> s{nextPseudoLabel = nextPseudoLabel + 1})

    let injectThis = [SPseudoLabelDecl pseudoLabel, SOpInjection $ OStringInCode{text, locText = GeneratedCode}]
    modify' $ \s@UnwrapPassState{use2Ast} -> s{use2Ast = M.insertWith (flip (++)) curUseDecl injectThis use2Ast}
    pure $ SPseudoLabelRef pseudoLabel

  reduceWrapArg (SWrapInLabel a) = reduceWrapArg1 a
  reduceWrapArg arg = pure arg

  reduceWrapI i@SInstruction{args} = do
    args' <- mapM reduceWrapArg args
    pure $ Just (i :: SLine){args = args'}
  reduceWrapI SWrapLabelUseSection{useWrapSection} = modify' (\s -> s{curUseDecl = useWrapSection}) >> pure Nothing
  reduceWrapI anyOtherInstruction = pure $ Just anyOtherInstruction

patchUnwrapArgs :: M.Map T.Text SAST -> SAST -> SAST
patchUnwrapArgs use2AST = concatMap handleI
 where
  handleI SWrapLabelSection{declWrapSection} = fromJust $ M.lookup declWrapSection use2AST
  handleI i = [i]

labelError :: a
labelError = error "Pseudo label stuff should be reduced by now"

data Labels = Labels {realLabels :: M.Map String Int32, pseudoLabels :: MI.IntMap Int32}
labelingPass :: SAST -> Labels
labelingPass = snd . foldl' handleI (0, Labels{realLabels = M.empty, pseudoLabels = MI.empty})
 where
  handleI :: (Int32, Labels) -> SLine -> (Int32, Labels)
  handleI (addr, label2addr@Labels{realLabels}) SLabelDecl{labelName} = (addr, label2addr{realLabels = M.insert labelName addr realLabels})
  handleI (addr, label2addr@Labels{pseudoLabels}) SPseudoLabelDecl{labelId} = (addr, label2addr{pseudoLabels = MI.insert labelId addr pseudoLabels})
  handleI (addr, label2addr) (SOpInjection OStringInCode{text}) = (addr + fromIntegral (T.length text), label2addr)
  handleI (addr, label2addr) (SOpInjection OAlignDecl{alignTo}) = (addr + (alignTo - addr) `mod` alignTo, label2addr)
  handleI (addr, label2addr) SInstruction{} = (addr + 4, label2addr)
  handleI a SWrapLabelUseSection{} = a
  handleI a SWrapLabelSection{} = a

reduceASTPass :: Labels -> SAST -> AssemblerMonad LAST
reduceASTPass Labels{realLabels, pseudoLabels} = mapMaybeM labelI
 where
  labelI :: SLine -> AssemblerMonad (Maybe LLine)
  labelI SLabelDecl{} = pure Nothing
  labelI SPseudoLabelDecl{} = pure Nothing
  labelI SWrapLabelSection{} = pure Nothing
  labelI SWrapLabelUseSection{} = pure Nothing
  labelI SInstruction{instructionName, args = sargs, locInstructionName, locArgs} = do
    args' <- zipWithM reduceArg sargs locArgs
    pure $ Just $ LInstruction{instructionId = instructionName, args = args', locInstructionName, locArgs}
  labelI (SOpInjection w) = pure $ Just $ LOpInjection w

  reduceArg :: SArg -> SLocation -> AssemblerMonad LArg
  reduceArg (SImmediate i) _loc = pure $ LImmediate i
  reduceArg (SRegister r) _loc = pure $ LRegister r
  reduceArg (SLabelRef name) loc = do
    address <-
      maybe (newError loc ("Undeclared label " ++ name) >> failAssembly) pure $
        M.lookup name realLabels
    pure $ LLabelRef address
  -- this cannot fail, fromJust is ok
  reduceArg (SPseudoLabelRef labelId) _loc = pure $ LLabelRef $ fromJust $ MI.lookup labelId pseudoLabels
  reduceArg (SWrapInLabel _) _loc = labelError

newtype Assembler' = Assembler' {i2enc :: M.Map String InstructionDef}
getAssembler' :: Assembler -> Assembler'
getAssembler' Assembler{isa} = Assembler'{i2enc = M.fromList $ zip (map name isa) isa}

stimesM :: (Monoid a, Integral b) => b -> a -> a
stimesM 0 = const mempty
stimesM n = stimes n

assemblePass :: Assembler' -> LAST -> AssemblerMonad BL.ByteString
assemblePass Assembler'{i2enc} = fmap (toLazyByteString . mconcat) . flip evalStateT 0 . mapM encodeI
 where
  encodeI :: LLine -> StateT Int32 AssemblerMonad Builder
  encodeI LInstruction{instructionId, args, locInstructionName, locArgs} = flip mplus (pure mempty) $ case instructionId `M.lookup` i2enc of
    Nothing -> lift $ newError locInstructionName ("Unknown instruction " ++ instructionId) >> failAssembly
    Just coder -> do
      ip <- get
      put (ip + 4)
      encoded <- lift $ encoder coder $ EncodeMe{instructionId, args, locInstructionName, locArgs, instructionPointer=ip}
      pure $ word32LE encoded
  encodeI (LOpInjection (OStringInCode{text})) = do
    -- FIXME: this does not store the correct offset
    pure $ T.encodeUtf8Builder text
  encodeI (LOpInjection (OAlignDecl{alignTo})) = do
    ip <- get
    let alignBytes = (alignTo - ip) `mod` alignTo
    put $ ip + alignBytes
    pure $ stimesM ((alignTo - ip) `mod` alignTo) $ word8 0

assemble :: Assembler -> SAST -> AssemblerMonad (BL.ByteString, LAST)
assemble asm src = do
  let (ast', use2Ast) = unwrapWrapArgs src
  let ast'' = patchUnwrapArgs use2Ast ast'

  let labels = labelingPass ast''
  lAST <- reduceASTPass labels ast''
  let asm' = getAssembler' asm
  r <- assemblePass asm' lAST
  pure (r, lAST)