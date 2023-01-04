{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Genas where

import Data.List
import Data.Char
import Data.Text (Text)
import Text.Megaparsec hiding (Label)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as S
import qualified Data.Map as M
import Control.Monad

data ASTId p = ASTId String p
data ASTArglist p = ASTArglist [AstArg p] p
data AstArg p = Number Integer p | Register String p | LabelRef String p
data ASTLine p = Label (ASTId p) p | Op (ASTId p) (ASTArglist p) p
data AstTU p = AstTU [ASTLine p] p

class ASTNode n where
    getPosition :: n p -> p
instance ASTNode ASTId where
    getPosition (ASTId n p) = p
instance ASTNode ASTArglist where
    getPosition (ASTArglist args p) = p
instance ASTNode AstArg where
    getPosition (Number i p) = p
    getPosition (Register r p) = p
    getPosition (LabelRef l p) = p
instance ASTNode ASTLine where
    getPosition (Label i p) = p
    getPosition (Op o a p) = p
instance ASTNode AstTU where
    getPosition (AstTU l p) = p

data SourceSpan = SourceSpan { startPos :: SourcePos, endPos :: SourcePos }

instance Show (ASTId p) where
    show (ASTId n pn) = n
instance Show (ASTArglist p) where
    show (ASTArglist args pargs) = intercalate ", " (map show args)
instance Show (AstArg p) where
    show (Number n p) = "$" ++ show n
    show (Register r p) = r
    show (LabelRef r p) = ":" ++ r
instance Show (ASTLine p) where
    show (Label ide p2) = (show ide) ++ ":"
    show (Op opN (ASTArglist [] pa) p) = show opN
    show (Op opN args p) = show opN ++ " " ++ show args
instance Show (AstTU p) where
    show (AstTU lines p) = lines >>= ((++ "\n") . show)

data Error p = Error String p
asmError :: String -> p -> Error p
asmError = Error
asmErrorL p s = Left $ asmError s p

instance Show (Error SourceSpan) where
    show (Error msg (SourceSpan start end)) =
        (show (unPos $ sourceLine start)) ++ ":" ++ (show (unPos $ sourceColumn start)) ++ ": " ++ msg ++ "\n"

type AsmResult1 p r = Either (Error p) r 
type AsmResult p = AsmResult1 p B.Builder

type EncodeFunc p = Maybe (M.Map String Int) -> ASTArglist p -> AsmResult p
data Opcode p = Opcode { names :: [String]
                       , encode :: (EncodeFunc p) }
data Assembler p = Assembler { opcodes :: [Opcode p] }

type OpMap p = M.Map String (EncodeFunc p) 

normalizeInstrName :: String -> String
normalizeInstrName = map toLower

encode1 :: M.Map String (EncodeFunc p) -> Maybe (M.Map String Int) -> (ASTLine p) -> AsmResult p
encode1 opName2B labels (Op (ASTId name pName) args opP) = do
    encoder <- maybeToRight (asmError ("Unknown instruction " ++ name) pName) $ M.lookup (normalizeInstrName name) opName2B
    encoder labels args
encode1 opName2B labels (Label l p) = return $ mempty

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight left Nothing = Left left
maybeToRight left (Just r) = Right r

computeOpMapping :: Assembler p -> M.Map String (EncodeFunc p)
computeOpMapping Assembler { opcodes=oplist} = M.fromList $ oplist >>= (\op -> (map normalizeInstrName $ names op) `zip` (repeat $ encode op))

assemble :: Assembler p -> AstTU p -> Either (Error p) B.Builder
assemble as tu@(AstTU ops tuP) = do
    let opM = (computeOpMapping as)
    labels <- labelPass as tu
    mconcat <$> (mapM (encode1 opM (Just labels)) ops)

type AsmExtractor p r = AstArg p -> Maybe (M.Map String Int) -> AsmResult1 p r

unpack0 :: ASTArglist p -> AsmResult1 p ()
unpack0 (ASTArglist [] pA) = return ()
unpack0 (ASTArglist l pA) = asmErrorL pA $ "Expected 0 arguments, but got " ++ show (length l)

unpack3 :: ((AsmExtractor p a), (AsmExtractor p b), (AsmExtractor p c)) -> Maybe (M.Map String Int) -> ASTArglist p -> AsmResult1 p (a, b, c)
unpack3 (e1, e2, e3) labels (ASTArglist [a1, a2, a3] pA) =
    do
        r1 <- e1 a1 labels
        r2 <- e2 a2 labels
        r3 <- e3 a3 labels
        return (r1, r2, r3)
unpack3 ex labels (ASTArglist l pA) = asmErrorL pA $ "Expected 3 arguments, but got " ++ show (length l)

unpack1 :: ((AsmExtractor p a)) -> Maybe (M.Map String Int) -> ASTArglist p -> AsmResult1 p (a)
unpack1 (e1) labels (ASTArglist [a1] p) = e1 a1 labels
unpack1 ex labels (ASTArglist l pA) = asmErrorL pA $ "Expected 1 argument, but got " ++ show (length l)

unpack2 :: ((AsmExtractor p a), (AsmExtractor p b)) -> Maybe (M.Map String Int) -> ASTArglist p -> AsmResult1 p (a, b)
unpack2 (e1, e2) labels (ASTArglist [a1, a2] pA) =
    do
        r1 <- e1 a1 labels
        r2 <- e2 a2 labels
        return (r1, r2)
unpack2 ex labels (ASTArglist l pA) = asmErrorL pA $ "Expected 2 arguments, but got " ++ show (length l)

defineOp0 :: [String] -> B.Builder -> Opcode p
defineOp0 names encodeAs = Opcode { names=names, encode=(\labels -> \args -> encodeAs <$ unpack0 args) }

defineOp1 :: [String] -> ((AsmExtractor p a)) -> ((a) -> AsmResult p) -> Opcode p
defineOp1 names extractor encoder = Opcode { names=names, encode=(\labels -> \args -> unpack1 extractor labels args >>= encoder) }

defineOp2 :: [String] -> ((AsmExtractor p a), (AsmExtractor p b)) -> ((a, b) -> AsmResult p) -> Opcode p
defineOp2 names extractor encoder = Opcode { names=names, encode=(\labels -> \args -> unpack2 extractor labels args >>= encoder) }

defineOp3 :: [String] -> ((AsmExtractor p a), (AsmExtractor p b), (AsmExtractor p c)) -> ((a, b, c) -> AsmResult p) -> Opcode p
defineOp3 names extractor encoder = Opcode { names=names, encode=(\labels -> \args -> unpack3 extractor labels args >>= encoder) }

labelPass :: Assembler p -> AstTU p -> AsmResult1 p (M.Map String Int)
labelPass as (AstTU ops tuP) = snd <$> foldM update (0, M.empty) ops
    where encoder = ((S.length . S.toStrict . B.toLazyByteString) <$>) . encode1 (computeOpMapping as) Nothing
          update (adr, labels) (Label (ASTId lab labPos) lp) = Right $ (adr, M.insert lab adr labels)
          update (adr, labels) op = (, labels) <$> (adr +) <$> encoder op