module TopKTest (
      topKTests
    ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import TopK


topKTests :: Test
topKTests = testGroup "topK"
    [ testGroup "timeless topK" [
        testProperty "overall count equals input count" prop_overall_count_equals_input_count
      , testProperty "overall count >= top k counts" prop_overall_count_greater_than_or_equal_to_top_k_counts
      ]
    , testGroup "accurate topK" [
        testProperty "overall count equals input count" prop_overall_count_equals_input_count
      , testProperty "overall count equals top k counts" prop_overall_count_equals_top_k_counts
      , testProperty "top counts are accurate" prop_top_counts_are_accurate
      ]
    ]

prop_overall_count_equals_input_count :: TimelessTopK -> Bool
prop_overall_count_equals_input_count (Timeless tk) =
  inputCount tk == fst (outputTopK tk)

prop_overall_count_greater_than_or_equal_to_top_k_counts :: TimelessTopK -> Bool
prop_overall_count_greater_than_or_equal_to_top_k_counts (Timeless tk) =
  inputCount tk >= sum (map snd . snd $ outputTopK tk)

prop_overall_count_equals_top_k_counts :: AccurateTopK -> Bool
prop_overall_count_equals_top_k_counts (Accurate tk) =
  inputCount tk == sum (map snd . snd $ outputTopK tk)

prop_top_counts_are_accurate :: AccurateTopK -> Bool
prop_top_counts_are_accurate (Accurate tk) =
  inputTop tk == snd (outputTopK tk)

data TimelessTopK = Timeless TopKTest.TopK
  deriving Show

instance Arbitrary TimelessTopK where
  arbitrary = do
    s <- choose (5,100)
    Timeless <$> generateTopK s 0

data AccurateTopK = Accurate TopKTest.TopK
  deriving Show

instance Arbitrary AccurateTopK where
  arbitrary = Accurate <$> generateTopK 100 0

data TopK = TopK {
    inputKey :: Key
  , inputData :: [Item]
  , inputSlots :: Slots
  , inputPeriod :: Time
  , inputCount :: Count
  , inputTop :: [(Element,Count)]
  , outputTopK :: (Count,[(Element,Count)])
} deriving Show

generateTopK :: Slots -> Period -> Gen TopKTest.TopK
generateTopK s p = do
    k <- arbitrary
    n <- oneof $ map return [1..100]
    es <- replicateM n $ choose ('A', 'Z')
    cs <- replicateM n $ choose (1, 10)
    ts <- replicateM n arbitrary
    let xs = map (\(e,c,NonNegative t) -> ([e],c,t)) $ zip3 es cs ts
        tk = foldl (\tk' x -> increment s p x tk') (create k p $ head xs) (tail xs)
    return $ TopKTest.TopK {
        inputKey = k
      , inputData = xs
      , inputSlots = s
      , inputPeriod = p
      , inputCount = sum cs
      , inputTop = sortBy (comparing ((*) (-1) . snd))
                         . map (\xs' -> ([fst (head xs')], sum $ map snd xs'))
                         . groupBy (\e1 e2 -> fst e1 == fst e2)
                         . sortBy (comparing fst) $ zip es cs
      , outputTopK = topK tk
    }