import Test.Hspec
import qualified Typecheck as T

main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers" $ do
    describe "Using a valid index to access a register" $ do
      it "Returns the content inside the register" $ do
        pending
        
  
