module Main where

import System.Random
import Control.Lens

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomPosition <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPosition xs
  fmap (a:) (shuffle (left ++ right))

finMyNumByIndex :: Int -> Int -> [Int] -> Int -> Bool
finMyNumByIndex index target listOfInts attempts =
  if attempts > 50 then False
  else
    if listOfInts ^?! element (index - 1) == target then True
    else finMyNumByIndex (listOfInts ^?! element (index - 1)) target listOfInts (attempts + 1)

allFound :: Int -> [Int] -> Bool
allFound target listOfInts =
  if target > 100 then True
  else
    if finMyNumByIndex target target listOfInts 1 then allFound (target + 1) listOfInts
    else False

main = do
  x <- shuffle [1..100]
  let c =  allFound 1 x
  print c
