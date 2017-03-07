{-# LANGUAGE GADTs        #-}

module SimpleVM.VM
    ( Program
    , VmState
    , runSimpleVm
    , VmError
    ) where

import Prelude hiding (print)
import qualified Data.Map as M
import Control.Monad.State

type Program = [Integer]    -- FIXME: replace with ByteString

data VmState = VmState { vmIp :: Int
                       , vmCode :: Program    
                       , vmStack :: [Integer]
                       , vmGlobals :: M.Map Integer Integer
                       , vmStopped :: Bool
                       , vmOutput :: [String]
                       , vmTrace :: [String]
                       }
                       deriving (Show)

makeSimpleVm :: Program -> VmState
makeSimpleVm code = VmState 0 code [] M.empty False [] []

data VmError where
    MemoryViolation :: VmError
    deriving Show

type VmSt a = State VmState a

data Operation where
    OpIAdd   :: Operation -- 1
    OpIConst :: Integer -> Operation -- 9
    OpPrint  :: Operation -- 14
    OpCall   :: Integer -> Integer -> Operation -- 16
    OpHalt   :: Operation -- 18

instance Show Operation where
    show (OpIAdd)       = "IADD"
    show (OpIConst v)   = "ICONST " ++ show v
    show (OpPrint)      = "PRINT"
    show (OpCall a1 a2) = "CALL " ++ show a1 ++ show a2
    show (OpHalt)       = "HALT"

advanceIp :: VmState -> VmState
advanceIp vm = vm { vmIp = (vmIp vm) + 1 }

halt :: VmState -> VmState
halt vm = vm { vmStopped = True }

print :: String -> VmState -> VmState
print s vm = vm { vmOutput = (vmOutput vm) ++ [s] }

printTrace :: String -> VmState -> VmState
printTrace s vm = vm { vmTrace = (vmTrace vm) ++ [s] }


fetchOne :: VmSt Integer
fetchOne = do
    vm <- get
    let value = vmCode vm !! vmIp vm    -- FIXME: out of bounds error
    modify advanceIp
    return value     

decode :: Integer -> VmSt Operation
decode opcode = do
    case opcode of
        1  -> return OpIAdd
        9  -> OpIConst <$> fetchOne
        14 -> return OpPrint
        16 -> OpCall <$> fetchOne <*> fetchOne
        18 -> return OpHalt


trace :: Operation -> VmSt ()
trace op = modify $ printTrace $ show op

push :: Integer -> VmSt ()
push v = modify $ push' v
    where push' v vm  = vm { vmStack = v : (vmStack vm) }

pop :: VmSt Integer
pop = do
    v <- gets $ head . vmStack
    modify (\vm -> vm { vmStack = tail (vmStack vm) }) -- FIXME: underflow error
    return v

exec :: Operation -> VmSt ()
exec (OpIAdd) = do
    v1 <- pop
    v2 <- pop
    push (v1 + v2)
exec (OpIConst v)   = push v
exec (OpPrint)      = do
    v <- pop
    modify $ print (show v)
exec (OpCall a1 a2) = undefined
exec (OpHalt)       = modify halt

cpu :: VmSt ()
cpu = do
    opcode <- fetchOne
    op <- decode opcode
    trace op
    exec op
    vm <- get
    if vmStopped vm then return ()
                    else cpu

runSimpleVm :: Program -> VmState
runSimpleVm code = execState cpu $ makeSimpleVm code

prog = [9, 2, 9, 3, 1, 14, 18]
