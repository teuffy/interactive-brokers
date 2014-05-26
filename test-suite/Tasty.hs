import Test.Tasty
import Test.Tasty.HUnit as HU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "IB test suite"
  [ testGroup "HUnit Tests" [hunitTest]
  ]

hunitTest :: TestTree
hunitTest = HU.testCase "HUnit test case" $ assertEqual "HUnit test failed" () ()

--import           Data.Attoparsec.Char8
--import qualified Data.ByteString       as B
--import           Data.ByteString.Char8 (ByteString, singleton)
--import qualified Data.ByteString.Char8 as BC
--import           Data.Char  
--import           API.IB.Parser
--print $ parse parseAccountSummary $ BC.pack "1\NUL0\NULDU88333\NULAccruedCash\NUL240.21\NULAUD\NUL"
--print $ parse parseAccountSummary $ BC.pack "1\NUL0\NULDU88333\NULBuyingPower\NUL349075.02\NULAUD\NUL"
--print $ parse parseAccountValue $ BC.pack "2\NULAccountCode\NULDU88333\NUL\NULDU88333\NUL"