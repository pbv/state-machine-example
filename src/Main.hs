
module Main where

import Test.QuickCheck
import QueueTest

main = do
  putStrLn "*** sequential tests ***"
  quickCheck prop_sequential
  putStrLn "*** parallel tests ***"
  quickCheck prop_parallel
  
