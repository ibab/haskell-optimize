{-# LANGUAGE RankNTypes #-}

module Numeric.Optimization where

import Numeric.AD
import Numeric.AD.Types hiding ((^*),(*^))
import Control.Applicative

import Linear.Vector
import Linear.Metric
import Linear.Matrix

instance Metric [] where
  dot x y = sum $ zipWith (*) x y

eye :: Num a => Int -> [[a]]
eye n = do
  i <- [0..n-1]
  return $ replicate i 0 ++ [1] ++ replicate (n - i - 1) 0

-- | Simple armijo backtracking line search with fixed factor of c1=1e-4
backtrack :: (Num a, Fractional a, Ord a) => (forall s. Mode s => [AD s a] -> AD s a) -> [a] -> [a] -> a -> a
backtrack f x p initAlpha = until armijoCondition (* 0.8) initAlpha
  where armijoCondition alpha = fx_ <= fx + 1e-4 * alpha * (gf `dot` p)
          where (fx,  gf ) = grad' f x
                (fx_, gf_) = grad' f $ x ^+^ (alpha *^ p)

-- | Broyden-Fletcher-Goldfarb-Shanno algorithm
--
-- Finds the minimum of f by iteratively updating an approximation to the (inverse) Hessian.
bfgs :: (Num a, Fractional a, Ord a) => (forall s. Mode s => [AD s a] ->  AD s a) -> [a] -> [[a]]
bfgs f x0 = go f x0 (grad f x0) (eye $ length x0)
  where go :: (Num a, Fractional a, Ord a) => (forall s. Mode s => [AD s a] -> AD s a) -> [a] -> [a] -> [[a]] -> [[a]]
        go f x gf h = if sum (map abs gf) < 1e-16 then []
                         else let -- step direction
                                  p     = map negate $ h !* gf
                                  -- calculate step size by line search
                                  alpha = backtrack f x p 1
                                  -- new position
                                  x_    = x ^+^ (alpha *^ p)
                                  -- new gradient
                                  gf_   = grad f x_
                                  -- helper variables
                                  s     = x_ ^-^ x
                                  y     = gf_ ^-^ gf
                                  rho   = 1 / (y `dot` s)
                                  -- update inverse hessian
                                  h_    = (eye (length x0) !-! rho *!! (s `outer` y)) !*! h !*! (eye (length x0) !-! rho *!! (y `outer` s)) !+! rho *!! (s `outer` s)
                              in (x:go f x_ gf_ h_)

