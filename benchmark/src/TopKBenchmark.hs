module TopKBenchmark (
      topKBenchmarks
    ) where

import Criterion.Main
import Data.List (foldl')
import TopK


topKBenchmarks :: Benchmark
topKBenchmarks =
  let lotsOfData = take 10000 infiniteData
      fewSlots = 10
      lotsOfSlots = 200
  in
  bgroup "topK"
    [ bench "insert 10000 with few slots" $ whnf (insert fewSlots) lotsOfData
    , bench "insert 10000 with lots of slots" $ whnf (insert lotsOfSlots) lotsOfData
    ]

insert :: Slots -> [(Period, Item)] -> TopK
insert s ((p',i'):xs) =
  foldl' (\tk (p,i)-> increment s p i tk) (create "X" p' i') xs
insert _ [] = error "no data"

infiniteData :: [(Period, Item)]
infiniteData =
  zip [0..] $ zip3 (map show $ cycle [(1::Int)..1000])
                   (cycle [1..3])
                   [500..]