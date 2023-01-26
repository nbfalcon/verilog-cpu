{-# LANGUAGE OverloadedStrings #-}

module HSBU.Genas.Dialect.MIPSLike where

import Data.Text
import Data.Void
import HSBU.Genas.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

identChar1 :: Parser Char
identChar1 = alphaNumChar <|> char '_'

identChar2 :: Parser Char
identChar2 = identChar1 <|> digitChar

p1 *:> p2 = ((:) <$> p1) <*> p2

ident :: Parser String
ident = identChar1 *:> many identChar2

registerE = char '$' *> (ident <|> many digitChar)

many1 p = p *:> many p

numberE :: Parser Integer
numberE = read <$> many1 numberChar

labelRefE :: Parser String
labelRefE = ident

expression =
    (SImmediate <$> numberE)
        <|> (SRegister <$> registerE)
        <|> (SLabelRef <$> labelRefE)

skipSpace' :: Parser () -> Parser ()
skipSpace' spc = L.space spc (L.skipLineComment "//" <|> L.skipLineComment "#" <|> L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lexeme1 :: Parser a -> Parser a
-- Don't skip newlines
lexeme1 = L.lexeme $ skipSpace' hspace1

-- Skip everything
lexeme2 = L.lexeme $ skipSpace' space1

instructionArgs = lexeme1 expression `sepBy` lexeme2 (char ',')

instruction = do
    instructionName <- lexeme1 ident
    args <- instructionArgs
    return SInstruction{instructionName = instructionName, args = args}

labelDeclId = ident <* char ':'

line = try (SLabelDecl <$> labelDeclId) <|> instruction

sourceFile = skipSpace' space1 *> many (lexeme2 line)