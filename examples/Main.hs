
import Control.Monad
import Numeric.Optimization

main = do
  forM_ (bfgs (\xs -> sum $ zipWith (*) [1..10] (map (**2) (zipWith (+) xs [-5..5]))) (replicate 10 0)) $ \x -> do
    putStrLn $ show x

