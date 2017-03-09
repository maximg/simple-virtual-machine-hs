{-# LANGUAGE GADTs        #-}

module SimpleVM.Asm
    ( compile
    , generate
    , makeSymbols
    ) where

import SimpleVM.VM
import Data.Maybe
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (spaces)

data JumpTarget where
    JTAddr :: Integer -> JumpTarget
    JTLabel :: String -> JumpTarget
    deriving (Show)

type Symbols = M.Map String Integer

data Statement where
    StOperation :: Operation -> Statement
    StJumpOp    :: (Integer -> Operation) -> JumpTarget -> Statement
    StLabelled  :: String -> Statement -> Statement
    StNone      :: Statement

instance Show Statement where
    show (StOperation op)  = show op
    show (StJumpOp op t)   = show (op 0) ++ "(target: " ++ show t ++ ")"
    show (StLabelled l st) = l ++ ": " ++ show st
    show (StNone)          = "NOP"

stSize :: Statement -> Integer
stSize (StLabelled _ st)  = stSize st
stSize (StOperation op)   = opSize op
stSize (StJumpOp op _)    = opSize (op 0)
stSize (StNone)           = 0

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

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

jumpTarget :: Parser JumpTarget
jumpTarget = try (JTAddr <$> integer)
         <|> try (JTLabel <$> identifier)
         <?> "jump target"

parseOp0 :: String -> Operation -> Parser Statement
parseOp0 opcode op = (StOperation op) <$ string opcode

parseOp1 :: String -> (Integer -> Operation) -> Parser Statement
parseOp1 opcode op = do
    string opcode
    spaces
    x <- integer
    return $ StOperation (op x)

parseJump1 :: String -> (Integer -> Operation) -> Parser Statement
parseJump1 opcode op = do
    string opcode
    spaces
    x <- jumpTarget
    return (StJumpOp op x)        

op :: Parser Statement
op =    try (parseOp0 "IADD"   OpIAdd  )
    <|> try (parseOp0 "ISUB"   OpISub  )
    <|> try (parseOp0 "IMUL"   OpIMul  )
    <|> try (parseOp0 "LT"     OpLt    )
    <|> try (parseOp0 "EQ"     OpEq    )
    <|> try (parseJump1 "BRT"  OpBrt   )
    <|> try (parseJump1 "BRF"  OpBrf   )
    <|> try (parseJump1 "BR"   OpBr    )
    <|> try (parseOp1 "ICONST" OpIConst)
    <|> try (parseOp1 "GLOAD"  OpGLoad )
    <|> try (parseOp1 "GSTORE" OpGStore)
    <|> try (parseOp1 "ICONST" OpIConst)
    <|> try (parseOp0 "PRINT"  OpPrint )
    <|> try (parseOp0 "POP"    OpPop   )
    <|> try (parseOp0 "HALT"   OpHalt  )    
    <?> "opcode"

labelled :: Parser Statement
labelled = do
    l <- identifier
    char ':'
    op <- operation
    return $ StLabelled l op

operation :: Parser Statement
operation = space *> spaces *> op

statement :: Parser Statement
statement =
        try labelled
    <|> try operation
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
generate prog = let symbols = makeSymbols prog in
    concatMap (genStatement symbols) prog

makeSymbols :: [Statement] -> Symbols
makeSymbols xs = go M.empty 0 xs
    where
        go :: Symbols -> Integer -> [Statement] -> Symbols
        go symbols _ [] = symbols
        go symbols n ((StLabelled s st):rest) =
            go (M.insert s n symbols) (n + stSize st) rest -- FIXME: duplicate labels
        go symbols n (st:rest) = go symbols (n + stSize st) rest

genStatement :: Symbols -> Statement -> [Integer]
genStatement _       (StOperation op) = genOp op
genStatement symbols (StJumpOp op target) = genOp (op $ resolve target)
    where
        resolve (JTAddr addr) = addr
        resolve (JTLabel v) = case M.lookup v symbols of
            Nothing   -> error $ "Not found: " ++ v     -- FIXME: addr not found
            Just addr -> addr
genStatement symbols (StLabelled _ op) = genStatement symbols op
genStatement _ _ = []

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
