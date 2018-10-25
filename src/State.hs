{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import System.Random
import Control.Applicative(liftA2, liftA3)
import Control.Monad(replicateM)
import Control.Monad.Trans.State

rollDie :: State StdGen Int
rollDie = state (randomR (1, 6))

rollsToGetTwenty :: State StdGen Int
rollsToGetTwenty =
  let go :: Int -> Int -> Int -> State StdGen Int
      go count rolls num
        | count >= 20 = pure rolls
        | otherwise = rollDie >>= go (count+num) (rolls+1)
  in rollDie >>= go 0 1

rollsToGetN :: Int -> State StdGen Int
rollsToGetN n =
  let go :: Int -> Int -> Int -> State StdGen Int
      go count rolls num
        | count >= n = pure rolls
        | otherwise = rollDie >>= go (count+num) (rolls+1)
  in rollDie >>= go 0 1

rollsCountLogged :: Int -> State StdGen (Int, [Int])
rollsCountLogged n =
  let go :: Int -> Int -> [Int] -> State StdGen (Int, [Int])
      go count roll_count rolls
        | count >= n = pure (roll_count, rolls)
        | otherwise = rollDie >>= \num -> go (count+num) (roll_count+1) (rolls++[num])
  in do
    r <- rollDie
    go r 1 [r]


newtype MyState s a = MyState { runstate :: s -> (a, s) }

instance Functor (MyState s) where
  fmap :: (a -> b) -> MyState s a -> MyState s b
  fmap f g = MyState $ \s -> let (a', s') = (runstate g) s
                              in (f a', s')

instance Applicative (MyState s) where
  pure x = MyState $ \s -> (x, s)
  f <*> g = MyState $ \s -> let (f', s1) = runstate f s
                                (a, s2) = runstate g s1
                            in (f' a, s2)

instance Monad (MyState s) where
  return = pure
  (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  f >>= ag = MyState $ \s -> let (a, s1)  = runstate f s
                             in runstate (ag a) s1

evalstate :: MyState s a -> s -> a
evalstate f s = let (res, _) = runstate f s
                in res

-- StdGen -> (Int, StdGen)
randomInt :: MyState StdGen Int
randomInt = MyState next

--Int -> StdGen -> ([Int], StdGen)
getNInts :: Int -> MyState StdGen [Int]
getNInts 0 = pure []
getNInts n = (:) <$> randomInt <*> getNInts (n-1)

getInts :: MyState StdGen [Int]
getInts = randomInt >>= getNInts

infiniteRandom :: MyState StdGen [Int]
infiniteRandom = liftA2 (:) randomInt infiniteRandom
