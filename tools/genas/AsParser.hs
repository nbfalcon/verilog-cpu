{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module AsParser where

import Genas
import Data.Text
import Data.Char
import Data.Void
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data SourceSpan = SourceSpan { startLoc :: SourcePos, endLoc :: SourcePos }

instance AstPosition SourceSpan where
    spanning SourceSpan { startLoc } SourceSpan { endLoc } = SourceSpan { startLoc, endLoc }
    after SourceSpan { endLoc=startLoc } sp2 = sp2 { startLoc }

instance Show (SourceSpan) where
    show SourceSpan { startLoc, endLoc } = (sourceName startLoc) ++ ":"
        ++ (show $ unPos $ sourceLine startLoc) ++ ":" ++ (show $ unPos $ sourceColumn startLoc)
        -- ++ "-" ++ (show $ unPos $ sourceLine endLoc) ++ ":" ++ (show $ unPos $ sourceColumn endLoc)

type AST = AstLine SourceSpan

spanned :: (TraversableStream s, MonadParsec e s m) => m (SourceSpan -> b) -> m b
spanned baseParser = do
    start <- getSourcePos
    v <- baseParser
    end <- getSourcePos
    return (v $ SourceSpan start end)

type Parser = Parsec String Text

scP :: Bool -> Parser ()
scP b = L.space
    (skipSome $ satisfy $ \c -> (b || c /= '\n') && isSpace c)
    (try (L.skipLineComment "//") <|> try (L.skipLineComment "#") <|> try (L.skipLineComment ";"))
    (L.skipBlockComment "/*" "*/")

scExpr :: Parser ()
scExpr = scP False

scNl :: Parser ()
scNl = scP True

lexeme1 :: Parser a -> Parser a
lexeme1 = L.lexeme scExpr

lexeme2 :: Parser a -> Parser a
lexeme2 = L.lexeme scNl

ident :: Parser String
ident = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> (char '_'))

num :: Parser Integer
num = do char '$'; L.decimal

labelRefP :: Parser String
labelRefP = do char ':'; ident

labelLine = (Label <$>) $ lexeme2 $ do i <- ident; char ':'; return i 

arg :: Parser (AstArg SourceSpan)
arg = spanned p1
    where p1 = (try (Number <$> num)) <|> (try (Register <$> ident)) <|> (LabelRef <$> labelRefP)

opParser = do
    name <- spanned $ AstId <$> lexeme1 ident
    args <- spanned $ AstArgList <$> (lexeme1 arg) `sepBy` (lexeme2 $ char ',')
    scNl
    return $ Op (name) args

data StringConstantType = ASCIZ | ASCII

stringLiteral = (char '"' >> manyTill L.charLiteral (char '"')) <|> (char '\'' >> manyTill L.charLiteral (char '\''))

stringConstant = do
    constType <- lexeme2 $ (try $ ASCIZ <$ string ".asciz") <|> (try $ ASCII <$ string ".ascii")
    let lit = lexeme2 stringLiteral
    let lit' = case constType of
                ASCIZ -> (++ "\0")
                ASCII -> id
    AstStringConstant <$> lit' <$> lit 

astLine :: Parser (AstLine SourceSpan)
astLine = spanned $ (try opParser <|> try labelLine <|> try stringConstant)

src :: Parser (AstTU SourceSpan)
src = spanned $ do
    scNl
    tu <- AstTU <$> many astLine
    eof
    return tu