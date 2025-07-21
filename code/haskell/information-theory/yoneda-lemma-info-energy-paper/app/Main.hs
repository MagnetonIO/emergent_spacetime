module Main where

import InfoEnergy.Examples

main :: IO ()
main = do
  putStrLn "=== Yoneda-Theoretic Information-Energy Correspondence ==="
  putStrLn "Running examples from the paper...\n"
  runAllExamples
  putStrLn "\nAll examples completed successfully!"