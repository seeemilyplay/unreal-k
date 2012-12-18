module BenchmarkMain (main) where

import Criterion.Main
import TopKBenchmark

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
    [ topKBenchmarks
    ]