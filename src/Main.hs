
import Data.List
import System.Environment

solveRPN :: (Num a, Read a) => [String] -> a
solveRPN = head . foldl foldingFunc []
    where foldingFunc (x:y:ys) "*" = x * y:ys
          --foldingFunc (x:y:ys) "/" = x / y:ys
          foldingFunc (x:y:ys) "-" = x - y:ys
          foldingFunc (x:y:ys) "+" = x + y:ys
          foldingFunc xs numStr = read numStr:xs

main = do
  args <- getArgs
  if null args then
      putStrLn "rpn-calc\nReverse Polish Notation Calculator"
  else
      print $ solveRPN args
