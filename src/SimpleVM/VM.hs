{-# LANGUAGE GADTs        #-}

module SimpleVM.VM
    ( Program
    , Operation(..)
    , VmState(..)
    , VmError
    , runSimpleVm
    , opSize
    ) where

import Prelude hiding (print)
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Control.Lens

type Program = [Integer]    -- FIXME: replace with ByteString

data VmState = VmState { vmIp :: Integer
                       , vmFp :: Integer
                       , vmCode :: Program    
                       , vmStack :: [Integer]
                       , vmGlobals :: M.Map Integer Integer
                       , vmStopped :: Bool
                       , vmOutput :: [String]
                       , vmTrace :: [String]
                       }
                       deriving (Show)

makeSimpleVm :: Program -> VmState
makeSimpleVm code = VmState 0 0 code [] M.empty False [] []

data VmError where
    MemoryViolation :: VmError
    deriving Show

type VmSt a = State VmState a

data Operation where
    OpIAdd   :: Operation -- 1
    OpISub   :: Operation -- 2
    OpIMul   :: Operation -- 3
    OpLt     :: Operation -- 4
    OpEq     :: Operation -- 5
    OpBr     :: Integer -> Operation -- 6
    OpBrt    :: Integer -> Operation -- 7
    OpBrf    :: Integer -> Operation -- 8
    OpIConst :: Integer -> Operation -- 9
    OpLoad   :: Integer -> Operation -- 10
    OpGLoad  :: Integer -> Operation -- 11
    OpStore  :: Integer -> Operation -- 12
    OpGStore :: Integer -> Operation -- 13
    OpPrint  :: Operation -- 14
    OpPop    :: Operation -- 15
    OpCall   :: Integer -> Integer -> Operation -- 16 ; addr nargs
    OpRet    :: Operation -- 17
    OpHalt   :: Operation -- 18

instance Show Operation where
    show (OpIAdd)       = "IADD"
    show (OpISub)       = "ISUB"
    show (OpIMul)       = "IMUL"
    show (OpLt)         = "LT"
    show (OpEq)         = "EQ"
    show (OpBr  addr)   = "BR     " ++ show addr
    show (OpBrt addr)   = "BRT    " ++ show addr
    show (OpBrf addr)   = "BRF    " ++ show addr
    show (OpIConst v)   = "ICONST " ++ show v
    show (OpLoad v)     = "LOAD   " ++ show v
    show (OpGLoad v)    = "GLOAD  " ++ show v
    show (OpStore v)    = "STORE  " ++ show v
    show (OpGStore v)   = "GSTORE " ++ show v
    show (OpPrint)      = "PRINT"
    show (OpPop)        = "POP"
    show (OpCall a1 a2) = "CALL   " ++ show a1 ++ show a2
    show (OpRet)        = "RET"
    show (OpHalt)       = "HALT"

opSize :: Operation -> Integer
opSize (OpBr     _) = 2
opSize (OpBrt    _) = 2
opSize (OpBrf    _) = 2
opSize (OpIConst _) = 2
opSize (OpLoad   _) = 2
opSize (OpGLoad  _) = 2
opSize (OpStore  _) = 2
opSize (OpGStore _) = 2
opSize (OpCall _ _) = 3
opSize _            = 1

changeIp :: (Integer -> Integer) -> VmState -> VmState
changeIp f vm = vm { vmIp = f (vmIp vm) }

halt :: VmState -> VmState
halt vm = vm { vmStopped = True }

peek :: Integer -> VmState -> Integer
peek offset vm = (vmStack vm) !! (fromInteger offset)

poke :: Integer -> Integer -> VmState -> VmState
poke v offset vm = vm { vmStack = (element (fromInteger offset) .~ v) (vmStack vm) }

print :: String -> VmState -> VmState
print s vm = vm { vmOutput = (vmOutput vm) ++ [s] }

printTrace :: String -> VmState -> VmState
printTrace s vm = vm { vmTrace = (vmTrace vm) ++ [s] }


fetchOne :: VmSt Integer
fetchOne = do
    vm <- get
    let value = vmCode vm !! (fromInteger $ vmIp vm)   -- FIXME: out of bounds error
    modify $ changeIp (+ 1)
    return value     

decode :: Integer -> VmSt Operation
decode opcode = do
    case opcode of
        1  -> return OpIAdd
        2  -> return OpISub
        3  -> return OpIMul
        4  -> return OpLt
        5  -> return OpEq
        6  -> OpBr     <$> fetchOne
        7  -> OpBrt    <$> fetchOne
        8  -> OpBrf    <$> fetchOne
        9  -> OpIConst <$> fetchOne
        10 -> OpLoad   <$> fetchOne
        11 -> OpGLoad  <$> fetchOne
        12 -> OpStore  <$> fetchOne
        13 -> OpGStore <$> fetchOne
        14 -> return OpPrint
        15 -> return OpPop
        16 -> OpCall <$> fetchOne <*> fetchOne
        17 -> return OpRet
        18 -> return OpHalt
        _  -> error $ "Unsupported opcode " ++ show opcode


trace :: Operation -> VmSt ()
trace op = modify $ printTrace $ show op

push :: Integer -> VmSt ()
push v = modify (\vm -> vm { vmStack = v : (vmStack vm)
                           , vmFp = (vmFp vm) + 1
                           })

pop :: VmSt Integer
pop = do
    v <- gets $ head . vmStack
    modify (\vm -> vm { vmStack = tail (vmStack vm)
                      , vmFp = (vmFp vm) - 1
                      }) -- FIXME: underflow error
    return v

store :: Integer -> Integer -> VmSt ()
store v addr = do
    modify (\vm -> vm { vmGlobals = M.insert addr v $ vmGlobals vm})

fpOffset :: Integer -> Integer
fpOffset x | x >= 0 = x + 3
           | otherwise = x

execBinOp :: (Integer -> Integer -> Integer) -> VmSt ()
execBinOp f =  do
    v2 <- pop
    v1 <- pop
    push $ f v1 v2

exec :: Operation -> VmSt ()
exec (OpIAdd) = execBinOp (+)
exec (OpISub) = execBinOp (-)
exec (OpIMul) = execBinOp (*)
exec (OpLt)   = execBinOp (\x y -> if x < y  then 1 else 0)
exec (OpEq)   = execBinOp (\x y -> if x == y then 1 else 0)
exec (OpBr  addr)    = modify $ changeIp (\_ -> addr)
exec (OpBrt addr)    = pop >>= \v -> when (v == 1) $ modify $ changeIp (\_ -> addr)
exec (OpBrf addr)    = pop >>= \v -> when (v == 0) $ modify $ changeIp (\_ -> addr)
exec (OpIConst v)    = push v
exec (OpLoad idx)    = do
    fp <- gets vmFp
    v <- gets $ peek (fp - fpOffset idx - 1)
    push v
exec (OpGLoad addr)  = do
    mem <- gets vmGlobals
    push $ fromJust $ M.lookup addr mem -- FIXME: failed lookup error
exec (OpStore idx)   = do
    v <- pop
    fp <- gets vmFp
    modify $ poke v (fp - fpOffset idx - 1)
exec (OpGStore addr) = do
    v <- pop
    store v addr
exec (OpPrint)      = do
    v <- pop
    modify $ print (show v)
{- Frame structure:
    locals
    return addr
    old fp
    nargs
    args <- fp
-}
exec (OpCall addr nargs) = do
    fp <- gets vmFp
    modify (\vm -> vm { vmFp = 0 })
    push nargs
    push fp
    gets vmIp >>= push
    modify $ changeIp (\_ -> addr)
exec (OpPop)        = do
    pop
    return ()
exec (OpRet)        = do
    rval <- pop
    ip <- pop
    fp <- pop
    nargs <- pop
    modify (\vm -> vm { vmStack = drop (fromInteger nargs) (vmStack vm)
                      , vmFp = fp - nargs
                      , vmIp = ip
                      })
    push rval
exec (OpHalt)       = modify halt
-- exec op = error $ "Unsupported op " ++ show op

cpu :: VmSt ()
cpu = do
    op <- fetchOne >>= decode
    trace op
    exec op
    vm <- get
    if vmStopped vm then return ()
                    else cpu

runSimpleVm :: Program -> VmState
runSimpleVm code = execState cpu $ makeSimpleVm code
