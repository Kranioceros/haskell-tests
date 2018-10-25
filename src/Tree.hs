{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Tree where

import Data.Map(Map, (!), (!?))
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State

import Control.Applicative(Alternative(..))
import Text.Parser.Combinators
import Text.Trifecta

type Weight = Float

-- Full binary tree, with tags only in the leaves --
data BinT a = Leaf a
              | Junction (BinT a) (BinT a)
              deriving(Eq)

instance Show a => Show (BinT a) where
  show (Leaf a) = show a
  show (Junction lt rt) = "(* " ++ show lt ++ ' ' : show rt ++ ")"

instance Functor BinT where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Junction lt rt) = Junction (fmap f lt) (fmap f rt)

instance Foldable BinT where
  foldr :: forall a b . (a -> b -> b) -> b -> BinT a -> b
  -- es un foldr sobre el arbol en postorden --
  foldr f start tree =
    let go :: BinT a -> b -> b
        go (Leaf a) acc         = f a acc
        go (Junction lt rt) acc = go rt $ go lt acc
    in go tree start

-- A forest with binary trees and a counter --
data Forest a = Forest {
    getForest :: Map Weight [BinT a],
    getCount :: Int
    } deriving (Eq, Show)

emptyForest :: Forest a
emptyForest = Forest M.empty 0

instance Functor Forest where
  fmap f for@Forest{getForest=ts} = for{getForest = M.map (fmap . fmap $ f) ts}


-- Traverses the tree from top to bottom, adding a code to each leaf
withBitstrings :: BinT a -> BinT (a, String)
withBitstrings =
  let go :: String -> BinT a -> BinT (a, String)
      go str (Leaf a) = Leaf (a, str)
      go str (Junction lt rt) = Junction (go (str++"0") lt) (go (str++"1") rt)
  in go ""

rev (x, y) = (y, x)

mapTrees :: [(Weight, BinT a)] -> Forest a
mapTrees = foldr f emptyForest
  where
    f :: (Weight, BinT a) -> Forest a -> Forest a
    f t forest = execState (addTree t) forest


-- Gets the tree with smallest weight and removes it from the forest
getRemoveMin :: State (Forest a) (Weight, BinT a)
getRemoveMin = do
  Forest{getForest=f, getCount=c} <- get
  let (w, t) = head <$> M.findMin f
  let f' = M.update (\case
                        [t] -> Nothing
                        (t:ts) -> Just ts) w f
  put $ Forest f' (c-1)
  return (w, t)

addTree :: (Weight, BinT a) -> State (Forest a) ()
addTree (w, t) = do
  Forest{getForest=f, getCount=c} <- get
  put $ Forest (M.alter (add t) w f) (c+1)
  where add t Nothing = Just [t]
        add t (Just ts) = Just (t:ts)

-- Builds the Huffmann tree from the forest given
mkHuffmann :: State (Forest a) (BinT a)
mkHuffmann = do
  Forest{getForest=f, getCount=c} <- get
  if c < 2
    then return . snd $ (head <$> M.findMin f)
    else do
    (w1, t1) <- getRemoveMin
    (w2, t2) <- getRemoveMin
    addTree (w1+w2, Junction t1 t2)
    mkHuffmann

huffmannEncoding :: Ord a => [(Weight, a)] -> Map a String
huffmannEncoding as = M.fromList $ foldl (flip (:)) [] (huffmannTree as)

huffmannTree :: [(Weight, a)] -> BinT (a, String)
huffmannTree as =
  let forest = mapTrees $ (fmap . fmap) Leaf as
  in withBitstrings $ evalState mkHuffmann forest

encode :: Ord a => Map a String -> [a] -> Maybe String
encode m [] = Just ""
encode m ls = concat <$> traverse (m!?) ls

decode :: Ord a => Map a String -> String -> Maybe [a]
decode m mes = case runParser (many $ parseHuff m) mempty mes of
  Success res -> Just res
  Failure err -> Nothing

parseHuff :: Ord a => Map a String -> Parser a
parseHuff m =
  let m' = M.fromList . fmap rev . M.assocs $ m
      as = M.keys m'
      parseCode :: Parser String
      parseCode = foldr1 (<|>) . fmap string $ as
  in (m'!) <$> parseCode

test :: [(Float, Char)]
test = [(0.4, 'a'), (0.15, 'b'), (0.15, 'c'), (0.10, 'd'), (0.10, 'e'), (0.10, 'f')]
