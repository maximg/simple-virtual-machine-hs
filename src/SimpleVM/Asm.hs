{-# LANGUAGE GADTs        #-}

module SimpleVM.Asm
    ( AsmError(..)
    , compile
    , generate
    , makeSymbols
    ) where

import           SimpleVM.VM
import           Data.Maybe
import qualified Data.Map as M
import           Text.ParserCombinators.Parsec hiding (spaces, label)

type Label = String
type Symbols = M.Map Label Address

data Loc where
    LAddr  :: Address -> Loc
    LLabel :: Label -> Loc
    deriving (Show)

data Command where
    COp   :: Operation -> Command
    CJump :: Operation -> Loc -> Command
    CCall :: Loc -> Integer -> Command
    deriving (Show)

data Statement = Statement {
      stLabel   :: Maybe Label
    , stCommand :: Maybe Command
    }
    deriving (Show)

stSize :: Statement -> Integer
stSize (Statement _ Nothing)   = 0
stSize (Statement _ (Just cmd)) = cmdSize cmd
    where
        cmdSize (COp op) = opSize op
        cmdSize (CJump op _) = opSize op
        cmdSize (CCall _ _) = opSize (OpCall 0 0)

data AsmError where
    DuplicateSymbol :: Label -> Address -> Address -> AsmError
    deriving (Eq)

instance Show AsmError where
    show (DuplicateSymbol l a1 a2) =
        "address " ++ show a2 ++ ": label " ++ show l ++ " already defined at " ++ show a1

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

spaces :: Parser ()
spaces = skipMany (char ' ') >> return ()

unsigned :: Parser String
unsigned = many1 digit

signed :: Parser String
signed = (:) <$> char '-' <*> unsigned

integer :: Parser Integer
integer = read <$> (try signed <|> try unsigned)

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

comment :: Parser ()
comment = char ';' *> many (noneOf "\n\r") >> return ()

loc :: Parser Loc
loc =   try (LAddr <$> integer)
    <|> try (LLabel <$> identifier)
    <?> "location"

parseOp0 :: String -> Operation -> Parser Command
parseOp0 opcode op = (COp op) <$ string opcode

parseOp1 :: String -> (Integer -> Operation) -> Parser Command
parseOp1 opcode op = do
    string opcode
    many1 space
    arg <- integer
    return $ COp (op arg)

parseJump1 :: String -> (Integer -> Operation) -> Parser Command
parseJump1 opcode op = do
    string opcode
    many1 space
    l <- loc
    -- dummy jump addr in the op, will be resolved during codegen
    return $ CJump (op 0) l

parseCall :: Parser Command
parseCall = do
    string "CALL"
    many1 space
    l <- loc
    many1 space
    nargs <- integer
    return $ CCall l nargs

-- FIXME: replace with a more declarative opcode parsing,
-- like OpCode0 / OpCode1 / OpCode2 data and a dynamically built parser
command :: Parser Command
command =
        try (parseOp0   "IADD"   OpIAdd  )
    <|> try (parseOp0   "ISUB"   OpISub  )
    <|> try (parseOp0   "IMUL"   OpIMul  )
    <|> try (parseOp0   "LT"     OpLt    )
    <|> try (parseOp0   "EQ"     OpEq    )
    <|> try (parseJump1 "BRT"    OpBrt   )
    <|> try (parseJump1 "BRF"    OpBrf   )
    <|> try (parseJump1 "BR"     OpBr    )
    <|> try (parseOp1   "ICONST" OpIConst)
    <|> try (parseOp1   "LOAD"   OpLoad  )
    <|> try (parseOp1   "GLOAD"  OpGLoad )
    <|> try (parseOp1   "STORE"  OpStore )
    <|> try (parseOp1   "GSTORE" OpGStore)
    <|> try (parseOp1   "ICONST" OpIConst)
    <|> try (parseOp0   "PRINT"  OpPrint )
    <|> try (parseOp0   "POP"    OpPop   )
    <|> try (parseCall                   )
    <|> try (parseOp0   "RET"    OpRet   )
    <|> try (parseOp0   "HALT"   OpHalt  )
    <?> "opcode"

label :: Parser Label
label = identifier <* char ':'

commandPart :: Parser Command
commandPart = do
    space
    spaces
    cmd <- command
    return cmd

statement :: Parser Statement
statement = Statement
    <$> optionMaybe label
    <*> optionMaybe (try commandPart)
    <*  spaces
    <*  optional comment
    <?> "statement"

program :: Parser [Statement]
program = do
        x <- sepBy statement eol
        optional eol
        eof
        return x

compile :: String -> Either ParseError [Statement]
compile input = parse program "" input


makeSymbols :: [Statement] -> Either AsmError Symbols
makeSymbols xs = go M.empty 0 xs
    where
        go symbols _ [] = Right symbols
        go symbols n (st@(Statement (Just s) _):rest) =
            case M.lookup s symbols of
                Just n1 -> Left $ DuplicateSymbol s n n1
                Nothing -> go (M.insert s n symbols) (n + stSize st) rest
        go symbols n (st@(Statement Nothing _):rest) =
            go symbols (n + stSize st) rest

resolve :: Symbols -> Loc -> Address
resolve _       (LAddr addr) = addr
resolve symbols (LLabel v)   = case M.lookup v symbols of
    Nothing   -> error $ "Not found: " ++ v     -- FIXME: addr not found
    Just addr -> addr

genCommand :: Symbols -> Command -> [Integer]
genCommand _       (COp op)       = encode op
genCommand symbols (CJump op loc) = encode $ (unpack op) (resolve symbols loc)
    -- FIXME: better way to get opcode?
    where
        unpack (OpBr  _) = OpBr
        unpack (OpBrt _) = OpBrt
        unpack (OpBrf _) = OpBrf
genCommand symbols (CCall loc n)  = encode $ OpCall (resolve symbols loc) n

genStatement :: Symbols -> Statement -> [Integer]
genStatement symbols (Statement _ (Just cmd)) = genCommand symbols cmd
genStatement _       _                        = []

generate :: [Statement] -> Either AsmError [Integer]
generate prog = do
    symbols <- makeSymbols prog
    return $ concatMap (genStatement symbols) prog
