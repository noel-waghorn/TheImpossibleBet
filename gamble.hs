module Main where

import System.Random

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomPosition <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPosition xs
  fmap (a:) (shuffle (left ++ right))

findMyNumByIndex :: Int -> Int -> [Int] -> Int -> Bool
findMyNumByIndex index target listOfInts attempts =
  if attempts > 50 then False
  else
    let value = listOfInts !! (index - 1)
    in if value == target then True
       else findMyNumByIndex value target listOfInts (attempts + 1)

allFound :: Int -> [Int] -> Bool
allFound target listOfInts =
  if target > 100 then True
  else
    if findMyNumByIndex target target listOfInts 1 then allFound (target + 1) listOfInts
    else False

main :: IO ()
main = do
  x <- shuffle [1..100]
  print (allFound 1 x)
