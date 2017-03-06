{-# LANGUAGE GADTs        #-}

module SimpleVM.VM
    ( someFunc
    , Program
    , makeSimpleVm
    , cpu
    ) where

import qualified Data.Map as M
import Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Program = [Integer]    -- FIXME: replace with ByteString

data VmState = VmState { vmIp :: Int
                       , vmCode :: Program    
                       , vmStack :: [Integer]
                       , vmGlobals :: M.Map Integer Integer
                       , vmStopped :: Bool
                       , vmOutput :: String
                       }
                       deriving (Show)

makeSimpleVm :: Program -> VmState
makeSimpleVm code = VmState 0 code [] M.empty False ""

data VmError where
    MemoryViolation :: VmError
    deriving Show

type VmSt a = State VmState a

data Operation where
    OpIConst :: Integer -> Operation
    OpCall :: Integer -> Integer -> Operation
    OpHalt :: Operation


advanceIp :: VmState -> VmState
advanceIp vm = vm { vmIp = (vmIp vm) + 1 }

halt :: VmState -> VmState
halt vm = vm { vmStopped = True }

push :: Integer -> VmState -> VmState
push v vm = vm { vmStack = v : (vmStack vm) }


fetchOne :: VmSt Integer
fetchOne = do
    vm <- get
    let value = vmCode vm !! vmIp vm    -- FIXME: possible out of bounds error
    modify advanceIp
    return value     

decode :: Integer -> VmSt Operation
decode opcode = do
    case opcode of
        9 -> OpIConst <$> fetchOne
        18 -> return OpHalt
        16 -> OpCall <$> fetchOne <*> fetchOne


exec :: Operation -> VmSt ()
exec OpHalt = do
    modify halt
exec (OpIConst v) = do
    modify $ push v
exec (OpCall a1 a2) = undefined

cpu :: VmSt ()
cpu = do
    opcode <- fetchOne
    op <- decode opcode
    exec op
