import SqEq
import StrAtom

import Data.List (intercalate)

-- | Mmm... tasty test-suit...
import Test.Tasty       (defaultMain, testGroup, TestName)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
    defaultMain $
      testGroup "MAIN GROUP"
        [ testGroup "solveSqEq"
          [ caseGen 1 2 1 [-1.0]
          , caseGen 1 (-2) 1 [1.0]
          ]
        ]
  where
    caseNameGen :: Double -> Double -> Double -> TestName
    caseNameGen a b c =
      intercalate " ~> "
        [ saBrIfNeg a `saCnt` "x" `saDeg` "2" `saAdd`
          saBrIfNeg b `saCnt` "x"             `saAdd`
          saBrIfNeg c `saEqu` "0"
        , show $ solveSqEq a b c
        ]
    caseGen a b c = testCase (caseNameGen a b c) . (solveSqEq a b c @?=)
