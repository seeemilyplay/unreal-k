module TopKTest (
      topKTests
    ) where

import Control.Applicative
import Control.Monad
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import TopK


topKTests :: Test
topKTests = testGroup "topK"
    [ testGroup "timeless topK" [
        testProperty "overall count equals input count" prop_overall_count_equals_input_count
      ]
    ]

prop_overall_count_equals_input_count :: TimelessTopK -> Bool
prop_overall_count_equals_input_count tk =
  inputCount tk == fst (outputTopK tk)

data TimelessTopK = TimelessTopK {
    inputKey :: Key
  , inputData :: [Item]
  , inputSlots :: Slots
  , inputPeriod :: Time
  , inputCount :: Count
  , outputInstance :: TopK
  , outputTopK :: (Count,[(Element,Count)])
} deriving Show

instance Arbitrary TimelessTopK where
  arbitrary = do
    k <- arbitrary
    s <- choose (5,100)
    n <- oneof $ map return [1..100]
    es <- replicateM n $ choose ('A', 'Z')
    cs <- replicateM n $ choose (1, 10)
    ts <- replicateM n arbitrary
    let xs = map (\(e,c,NonNegative t) -> ([e],c,t)) $ zip3 es cs ts
        tk = foldl (\tk x -> increment s 0 x tk) (create k $ head xs) (tail xs)
    return TimelessTopK {
        inputKey = k
      , inputData = xs
      , inputSlots = s
      , inputPeriod = 0
      , inputCount = sum cs
      , outputInstance = tk
      , outputTopK = topK tk
    }
