module TestMain (main) where

import TopKTest
import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ topKTests
    ]