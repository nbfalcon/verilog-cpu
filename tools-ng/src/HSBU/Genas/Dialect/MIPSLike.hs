{-# LANGUAGE OverloadedStrings #-}

module HSBU.Genas.Dialect.MIPSLike where

import Data.Text as T
import Data.Void
import HSBU.Genas.AST
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

located :: Parser a -> Parser (a, SLocation)
located p = do
    start <- getSourcePos
    result <- p
    let sourceLine = unPos $ P.sourceLine start
    let sourceColumn = unPos $ P.sourceColumn start
    let sourceFileName = P.sourceName start
    pure (result, SLocation { sourceLine, sourceColumn, sourceFileName })

identChar1 :: Parser Char
identChar1 = alphaNumChar <|> char '_'

identChar2 :: Parser Char
identChar2 = identChar1 <|> digitChar

(*:>) :: Applicative f => f a -> f [a] -> f [a]
p1 *:> p2 = ((:) <$> p1) <*> p2

ident :: Parser String
ident = identChar1 *:> many identChar2

registerE :: Parser String
registerE = char '$' *> (ident <|> many digitChar)

many1 :: Parser a -> Parser [a]
many1 p = p *:> many p

numberE :: Read a =>Parser a
numberE = read <$> many1 numberChar

stringLiteral :: Parser String
stringLiteral = (char '"' >> manyTill L.charLiteral (char '"')) <|> (char '\'' >> manyTill L.charLiteral (char '\''))

stringLiteralT :: Parser Text
stringLiteralT = T.pack <$> stringLiteral

expression :: Parser SArg
expression
    = (SImmediate <$> numberE)
    <|> (SRegister <$> registerE)
    <|> (SLabelRef <$> ident)
    <|> (SWrapInLabel . SWrappedString <$> stringLiteralT)

skipSpace' :: Parser () -> Parser ()
skipSpace' spc = L.space spc (L.skipLineComment "//" <|> L.skipLineComment "#" <|> L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lexeme1 :: Parser a -> Parser a
-- Don't skip newlines
lexeme1 = L.lexeme $ skipSpace' hspace1

-- Skip everything
lexeme2 :: Parser a -> Parser a
lexeme2 = L.lexeme $ skipSpace' space1

instructionArgs :: Parser [(SArg, SLocation)]
instructionArgs = lexeme1 (located expression) `sepBy` lexeme2 (char ',')

instruction :: Parser SLine
instruction = do
    (instructionName, locInstructionName) <- lexeme1 $ located ident
    (args, locArgs) <- unzip <$> instructionArgs
    return SInstruction{instructionName, args, locInstructionName, locArgs}

labelDecl :: Parser SLine
labelDecl = do
    (labelName, locLabel) <- located $ ident <* char ':'
    return SLabelDecl { labelName, locLabel }

specialDecl :: Parser SLine
specialDecl
    = lexeme2 (string ".align") *> (SOpInjection . uncurry OAlignDecl <$> (lexeme2 $ located numberE))
    <|> lexeme2 (string ".ascii") *> (SOpInjection . uncurry OStringInCode <$> (lexeme2 $ located stringLiteralT))
    <|> lexeme2 (string ".asciz") *> (SOpInjection . uncurry OStringInCode <$> (lexeme2 $ located (T.pack . (++ "\0") <$> stringLiteral)))
    <|> lexeme2 (string ".use") *> (uncurry SWrapLabelUseSection <$> (located $ lexeme2 $ T.pack <$> ident))
    <|> lexeme2 (string ".wrapHere") *> (uncurry SWrapLabelSection <$> (located $ lexeme2 $ T.pack <$> ident))

line :: Parser SLine
line = specialDecl <|> try labelDecl <|> instruction

sourceFile :: Parser SAST
sourceFile = skipSpace' space1 *> many (lexeme2 line) <* eof