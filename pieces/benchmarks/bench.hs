module Main where
import Criterion.Main

doABarrelRoll :: [Int] -> Maybe Int
doABarrelRoll []     = Nothing
doABarrelRoll (x:xs) = Just x

main :: IO ()
main = defaultMain
  [ bench "empty barrel roll" $ nf doABarrelRoll []
  , bench "has some barrels" $ nf doABarrelRoll [1..10]
  , bench "such roll" $ nf doABarrelRoll [1..]
  ]
