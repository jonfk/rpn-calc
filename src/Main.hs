
import Data.List
import System.Environment
import Control.Monad (when)

solveRPN :: [String] -> Float
solveRPN = head . foldl foldingFunc []
    where foldingFunc (x:y:ys) "*" = x * y:ys
          foldingFunc (x:y:ys) "/" = x / y:ys
          foldingFunc (x:y:ys) "-" = x - y:ys
          foldingFunc (x:y:ys) "+" = x + y:ys
          foldingFunc xs "sum" = [sum xs]
          foldingFunc (x:xs) "ln" = log x:xs
          foldingFunc (x:y:ys) "^" = (y ** x):ys
          foldingFunc xs numStr = read numStr:xs

main = do
  args <- getArgs
  if null args then
      do
        putStrLn "Reverse Polish Notation Calculator"
        putStrLn "Operators suported: * / - + sum ln ^"
        putStrLn "Options:"
        putStrLn "\t--help: for help message"
  else
      do
        when (head args == "--help") $ do
          putStrLn "Operators suported: * / - + sum ln ^"
          putStrLn "Don't forget to escape \\* when using command line"
        print $ solveRPN args
