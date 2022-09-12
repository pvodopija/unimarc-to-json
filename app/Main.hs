module Main where

import System.Environment

import qualified MyLib (convert)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  putStrLn $ "Converting " ++ fileName ++ " to .json"
  input <- readFile fileName
  let output = MyLib.convert $ input
  case output of
    Left err -> putStrLn . show $ err
    Right conveted -> writeFile "output.json" $ conveted
