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
    , testGroup "any topK" [
        testProperty "overall count <= input count" prop_overall_less_than_or_equal_to_input_count
      ]
    ]

prop_overall_count_equals_input_count :: TimelessTopK -> Bool
prop_overall_count_equals_input_count (Timeless tk) =
  inputCount tk == (outputCount tk)

prop_overall_count_greater_than_or_equal_to_top_k_counts :: TimelessTopK -> Bool
prop_overall_count_greater_than_or_equal_to_top_k_counts (Timeless tk) =
  inputCount tk >= sum (map snd $ outputElements tk)

prop_overall_count_equals_top_k_counts :: AccurateTopK -> Bool
prop_overall_count_equals_top_k_counts (Accurate tk) =
  inputCount tk == sum (map snd $ outputElements tk)

prop_top_counts_are_accurate :: AccurateTopK -> Bool
prop_top_counts_are_accurate (Accurate tk) =
  inputTop tk == outputElements tk

prop_overall_less_than_or_equal_to_input_count :: AnyTopK -> Bool
prop_overall_less_than_or_equal_to_input_count (Any tk) =
  (outputCount tk) <= inputCount tk

data AnyTopK = Any TopKTest.TopK
  deriving Show

instance Arbitrary AnyTopK where
  arbitrary = do
    s <- choose (5,100)
    p <- choose (0,1000)
    Any <$> generateTopK s p

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
  , outputTopK :: (Count,Time,Time,[(Element,Count)])
  , outputCount :: Count
  , outputElements :: [(Element,Count)]
} deriving Show

generateTopK :: Slots -> Period -> Gen TopKTest.TopK
generateTopK s p = do
    k <- arbitrary
    n <- oneof $ map return [1..100]
    es <- replicateM n $ choose ('A', 'Z')
    cs <- replicateM n $ choose (1, 10)
    ts <- replicateM n arbitrary
    let xs = map (\(e,c,NonNegative t) -> ([e],c,t)) $ zip3 es cs ts
        tk = foldl (\tk' x -> add p x tk') (create s k $ head xs) (tail xs)
        result = topK 100000 tk
    return $ TopKTest.TopK {
        inputKey = k
      , inputData = xs
      , inputSlots = s
      , inputPeriod = p
      , inputCount = sum cs
      , inputTop = reverse . sortBy (comparing snd)
                           . map (\xs' -> ([fst (head xs')], sum $ map snd xs'))
                           . groupBy (\e1 e2 -> fst e1 == fst e2)
                           . sortBy (comparing fst) $ zip es cs
      , outputTopK = result
      , outputCount = (\(t,_,_,_) -> t) result
      , outputElements = (\(_,_,_,x) -> x) result
    }