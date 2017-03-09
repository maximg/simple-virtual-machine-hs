{-# LANGUAGE GADTs        #-}

module SimpleVM.Asm
    ( compile
    , generate
    ) where

import SimpleVM.VM
import Text.ParserCombinators.Parsec hiding (spaces)

data Statement where
    StOperation :: Operation -> Statement
    StNone      :: Statement
    deriving (Show)

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

spaces :: Parser ()
spaces = skipMany (char ' ') >> return ()

integer :: Parser Integer
integer = read <$> many1 digit

parseOp0 :: String -> Operation -> Parser Operation
parseOp0 opcode op = op <$ string opcode

parseOp1 :: String -> (Integer -> Operation) -> Parser Operation
parseOp1 opcode op = do
    string opcode
    spaces
    x <- integer
    return (op x)

op :: Parser Operation
op =    try (parseOp0 "IADD"   OpIAdd  )
    <|> try (parseOp0 "ISUB"   OpISub  )
    <|> try (parseOp0 "IMUL"   OpIMul  )
    <|> try (parseOp0 "LT"     OpLt    )
    <|> try (parseOp0 "EQ"     OpEq    )
    <|> try (parseOp1 "BR"     OpBr    )
    <|> try (parseOp1 "BRT"    OpBrt   )
    <|> try (parseOp1 "BRF"    OpBrf   )
    <|> try (parseOp1 "ICONST" OpIConst)
    <|> try (parseOp1 "GLOAD"  OpGLoad )
    <|> try (parseOp1 "GSTORE" OpGStore)
    <|> try (parseOp1 "ICONST" OpIConst)
    <|> try (parseOp0 "PRINT"  OpPrint )
    <|> try (parseOp0 "POP"    OpPop   )
    <|> try (parseOp0 "HALT"   OpHalt  )
    <?> "opcode"

operation :: Parser Operation
operation = space *> spaces *> op <* spaces

statement :: Parser Statement
statement =
        try (StOperation <$> operation)
    <|> (spaces >> return StNone)
    <?> "statement"

program :: Parser [Statement]
program = do
        x <- sepBy statement eol
        optional eol
        eof
        return x

compile :: String -> Either ParseError [Statement]
compile input = parse program "" input

generate :: [Statement] -> [Integer]
generate prog = concatMap genStatement prog

genStatement :: Statement -> [Integer]
genStatement (StOperation op) = genOp op
genStatement _ = []

genOp :: Operation -> [Integer]
genOp (OpIAdd)     = [1]
genOp (OpISub)     = [2]
genOp (OpIMul)     = [3]
genOp (OpLt)       = [4]
genOp (OpEq)       = [5]
genOp (OpBr  addr) = [6,  addr]
genOp (OpBrt addr) = [7,  addr]
genOp (OpBrf addr) = [8,  addr]
genOp (OpIConst v) = [9,  v]
genOp (OpGLoad v)  = [11, v]
genOp (OpGStore v) = [13, v]
genOp (OpPrint)    = [14]
genOp (OpPop)      = [15]
genOp (OpHalt)     = [18]
