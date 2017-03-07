{-# LANGUAGE GADTs        #-}

module SimpleVM.Asm
    ( compile
    , generate
    ) where

import SimpleVM.VM
import Text.ParserCombinators.Parsec hiding (spaces)

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
    <|> try (parseOp1 "ICONST" OpIConst)
    <|> try (parseOp0 "PRINT"  OpPrint )
    <|> try (parseOp0 "POP"    OpPop   )
    <|> try (parseOp0 "HALT"   OpHalt  )
    <?> "opcode"

statement :: Parser Operation
statement = space *> spaces *> op <* spaces

program :: Parser [Operation]
program =
    do x <- sepBy statement eol
       eof
       return x

compile :: String -> Either ParseError [Operation]
compile input = parse program "" input

generate :: [Operation] -> [Integer]
generate prog = concatMap genOp prog

genOp :: Operation -> [Integer]
genOp (OpIAdd)     = [1]
genOp (OpISub)     = [2]
genOp (OpIMul)     = [3]
genOp (OpIConst v) = [9, v]
genOp (OpPrint)    = [14]
genOp (OpPop)      = [15]
genOp (OpHalt)     = [18]
