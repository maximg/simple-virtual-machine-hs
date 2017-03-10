import Test.Hspec
import SimpleVM.VM
import SimpleVM.Asm
import qualified Data.Map as M

main :: IO ()
main = hspec $ do

  describe "halt" $ do
    it "stops program execution" $
      vmStopped (runProg [" HALT"]) `shouldBe` True


  describe "iconst" $ do
    it "pushes a value on top of the stack" $
      let prog = [" ICONST 1", " ICONST 2"] in
      vmStack (runProg prog) `shouldBe` [2,1]

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
      let prog = [ " CALL L1 0", " HALT", "L1: ICONST 55" ] in
      vmStack (runProg prog) `shouldBe` [55, 3, 0]

  describe "ret" $ do
    it "cleans up stack frame and returns execution to the caller" $
      let prog = [ " CALL L1 0", " HALT", "L1: ICONST 55", " RET" ] in
      vmStack (runProg prog) `shouldBe` [55]



runProg prog = case compile $ unlines (prog ++ [" HALT"]) of
                Left err -> error $ show err
                Right prog -> runSimpleVm $ generate prog
