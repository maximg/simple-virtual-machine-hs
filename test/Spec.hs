import Test.Hspec
import SimpleVM.VM
import SimpleVM.Asm
import Control.Exception
import Control.DeepSeq
import qualified Data.Map as M

main :: IO ()
main = hspec $ do

  describe "halt" $ do
    it "stops program execution" $
      vmStopped (runProg [" HALT"]) `shouldBe` True


  describe "iconst" $ do
    it "pushes a value on top of the stack" $
      let prog = [" ICONST 1", " ICONST -2"] in
      vmStack (runProg prog) `shouldBe` [-2,1]

  describe "pop" $ do
    it "removes the value from the top of the stack" $
      let prog = [ " ICONST 1", " POP" ] in
      vmStack (runProg prog) `shouldBe` []


  describe "iadd" $ do
    it "takes two values from the stack, adds them and places the result on the stack" $
      let prog = [ " ICONST 1", " ICONST 2", " IADD" ] in
      vmStack (runProg prog) `shouldBe` [3]

  describe "isub" $ do
    it "takes two values from the stack, subtracts one from another and places the result on the stack" $
      let prog = [ " ICONST 3", " ICONST 2", " ISUB" ] in
      vmStack (runProg prog) `shouldBe` [1]

  describe "imul" $ do
    it "takes two values from the stack, multiplies them and places the result on the stack" $
      let prog = [ " ICONST 3", " ICONST 2", " IMUL" ] in
      vmStack (runProg prog) `shouldBe` [6]


  describe "lt" $ do
    it "takes a and b from the stack and puts 1 on the stack when a < b" $
      let prog = [ " ICONST 1", " ICONST 2", " LT" ] in
      vmStack (runProg prog) `shouldBe` [1]

    it "takes a and b from the stack and puts 0 on the stack when a >= b" $
      let prog = [ " ICONST 2", " ICONST 1", " LT" ] in
      vmStack (runProg prog) `shouldBe` [0]


  describe "eq" $ do
    it "takes two values from the stack and puts 1 on the stack when they are equal" $
      let prog = [ " ICONST 1", " ICONST 1", " EQ" ] in
      vmStack (runProg prog) `shouldBe` [1]

    it "takes two values from the stack and puts 0 on the stack when they are not equal" $
      let prog = [ " ICONST 2", " ICONST 1", " EQ" ] in
      vmStack (runProg prog) `shouldBe` [0]


  describe "print" $ do
    it "takes a value from the stack and prints it on the output" $
      let prog = [ " ICONST 1", " PRINT" ] in
      vmOutput (runProg prog) `shouldBe` ["1"]


  describe "gstore" $ do
    it "takes a value from the stack and stores it in the global memory" $
      let prog = [ " ICONST 1", " GSTORE 2" ] in
      vmGlobals (runProg prog) `shouldBe` M.fromList [(2,1)]

  describe "gload" $ do
    it "takes a value from the global memory and puts it on the stack" $
      let prog = [ " ICONST 1", " GSTORE 2", " ICONST 3", " GLOAD 2" ] in
      vmStack (runProg prog) `shouldBe` [1, 3]

  describe "br" $ do
    it "unconditionally jumps to an address" $
      let prog = [ " BR 3", " HALT", " ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55]

  describe "brt" $ do
    it "takes a value from the stack and when it is 1 jumps to an address" $
      let prog = [ " ICONST 1", " BRT 5", " HALT", " ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55]

  describe "brf" $ do
    it "takes a value from the stack and when it is 0 jumps to an address" $
      let prog = [ " ICONST 0", " BRF 5", " HALT", " ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55]


  describe "label" $ do
    it "can be used instead of an address" $
      let prog = [ " BR L1", " HALT", "L1: ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55]


  describe "call" $ do
    it "stores current ip and number of arguments on the stack and jumps to an address" $
      let prog = [ " ICONST 44", " CALL L1 1", " HALT", "L1: ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55, 5, 1, 1, 44]

  describe "ret" $ do
    it "cleans up stack frame and returns execution to the caller" $
      let prog = [ " ICONST 44", " CALL L1 1", " HALT", "L1: ICONST 55", " RET" ] in
      vmStack (runProg prog) `shouldBe` [55]

  describe "load" $ do
    describe "given negative value" $ do
      it "takes an argument from current frame and puts it on the stack" $
        let prog = [ " ICONST 33", " CALL F 1", " HALT", "F: LOAD -1" ] in
        vmStack (runProg prog) `shouldBe` [33, 5, 1, 1, 33]

    describe "given non-negative value" $ do
      it "takes an local from current frame and puts it on the stack" $
        let prog = [ " CALL F 0", " HALT", "F: ICONST 55", " LOAD 0" ] in
        vmStack (runProg prog) `shouldBe` [55, 55, 3, 0, 0]

  describe "store" $ do
    describe "given negative value" $ do
      it "takes a value from the stack and stores it in an argument in current frame" $
        let prog = [ " ICONST 33", " CALL F 1", " HALT", "F: ICONST 44", " STORE -1" ] in
        vmStack (runProg prog) `shouldBe` [5, 1, 1, 44]

    describe "given non-negative value" $ do
      it "takes a value from the stack and stores it in a local in current frame" $
        let prog = [ " CALL F 0", " HALT", "F: ICONST 44", " ICONST 55", " STORE 0" ] in
        vmStack (runProg prog) `shouldBe` [55, 3, 0, 0]

  describe "assembler" $ do
    describe "when given duplicate labels" $ do
      it "produces an error message" $
        let prog = [ "L1: RET", "L1: RET" ] in
        assemble prog `shouldBe` Left (DuplicateSymbol "L1" 1 0)

  describe "vm" $ do
    describe "when ip runs out of bounds" $
      it "generates an error" $
        let prog = [ " BR 100" ] in
        (evaluate . force) (runProg prog) `shouldThrow` errorCall "Prelude.!!: index too large"

    describe "when the stack underflows" $
      it "generates an error" $
        let prog = [ " POP" ] in
        -- the error is generated by the tail and not by the head,
        -- as that one happens to be evaluated first
        evaluate (vmStack (runProg prog)) `shouldThrow` errorCall "Prelude.tail: empty list"

    describe "when reading an uninitialized global" $
      it "generates an error" $
        let prog = [ " GLOAD 0", " GSTORE 1" ] in
        (evaluate . force) (vmGlobals (runProg prog)) `shouldThrow` errorCall "Read from uninitialized global address 0"


-- required for `force` to work
instance NFData VmState where rnf x = seq x ()

assemble :: [String] -> Either AsmError Program
assemble prog = case compile $ unlines prog of
                  Left err -> error $ show err
                  Right obj -> generate obj

runProg :: [String] -> VmState
runProg prog = case assemble (prog ++ [" HALT"]) of
                Left err -> error $ show err
                Right prog -> runSimpleVm prog
