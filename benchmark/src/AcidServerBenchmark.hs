module AcidServerBenchmark (
      acidServerBenchmarks
    ) where

import Criterion.Main
import AcidServer


acidServerBenchmarks :: Benchmark
acidServerBenchmarks =
  let lotsOfData = take 10000 infiniteData
      fewSlots = 10
      lotsOfSlots = 200
  in
  bgroup "acid server"
    [ bench "insert 10000 with few slots" $ whnfIO (insert fewSlots lotsOfData)
    , bench "insert 10000 with lots of slots" $ whnfIO (insert lotsOfSlots lotsOfData)
    ]

insert :: Slots -> [(Key, Item)] -> IO ()
insert s xs = withAcidServer False s (\t -> t - 500) $ \acid ->
  mapM_ (\(k,v) -> save acid k v) xs

infiniteData :: [(Key, Item)]
infiniteData =
  zip (map show $ cycle [(1::Int)..5]) $
    zip3 (map show $ cycle [(1::Int)..1000])
         (cycle [1..3])
         [500..]