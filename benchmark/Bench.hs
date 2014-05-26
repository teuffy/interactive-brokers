module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified API.IBBench

main :: IO ()
main = defaultMain
    [ bgroup "API.IB" API.IBBench.benchmarks
    ]
