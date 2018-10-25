{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative(Alternative(..),liftA2, liftA3)
import Data.Maybe(isJust)
import Data.Ratio((%))
import Data.Map(Map)
import qualified Data.Map as M

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneEOF = one >> eof

onetwo :: Parser Char
onetwo = char '1' >> char '2'
onetwo' = onetwo >> stop
onetwoEOF = onetwo >> eof

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

onetwothree :: Parser String
--onetwothree = string "123" <|> string "12" <|> string "1"
onetwothree = choice [string "123", string "12", string "1"]

string' :: CharParsing m => String -> m String
string' = traverse char

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "onetwo:"
  testParse onetwo
  pNL "onetwo':"
  testParse onetwo'

-- My own parser combinators

newtype Parse a = Parse { runParse :: String -> (Maybe a, String) }

instance Functor Parse where
  fmap f p = Parse $ \s -> let (res, str) = runParse p s
                           in (fmap f res, str)

instance Applicative Parse where
  pure x = Parse $ \s -> (pure x, s)
  (<*>) :: Parse (a -> b) -> Parse a -> Parse b
  (Parse f) <*> (Parse g) = Parse $ \s ->
    let (f', s1) = f s
        (a', s2) = g s1
    in (f' <*> a', s2)

instance Alternative Parse where
  empty = Parse $ \s -> (Nothing, s)
  (<|>) :: Parse a -> Parse a -> Parse a
  (Parse f) <|> (Parse g) = Parse $ \s ->
    let (res1, s1) = f s
        (res2, s2) = g s
    in case (res1, res2) of
      (Just _, _) -> (res1, s1)
      (_, Just _) -> (res2, s2)
      _          -> (Nothing, s)

instance Monad Parse where
  return = pure
  (>>=) :: Parse a -> (a -> Parse b) -> Parse b
  (Parse f) >>= g = Parse $ \s ->
    let (a, s1) = f s
    in case a of
      Nothing -> (Nothing, s1)
      Just a' -> runParse (g a') s1
  fail _ = Parse $ \s -> (Nothing, s)

parseChr :: Char -> Parse Char
parseChr c = Parse $ \case
  [] -> (Nothing, [])
  s@(x:xs) -> if x == c
              then (Just x, xs)
              else (Nothing, s)

parseStr :: String -> Parse String
parseStr str =  Parse $ \s ->
    let (res, s') = runParse (go str) s
    in
      if isJust res
          then (res, s')
          else (res, s )
  where go :: String -> Parse String
        go "" = pure ""
        go (x:xs) = liftA2 (:) (parseChr x) (go xs)

some' :: Parse a -> Parse [a]
some' p = Parse $ \s ->
  let (res, s') = runParse p s
  in case res of
    (Just r) -> runParse ((r:) <$> many' p) s'
    Nothing -> (Nothing, s)

many' :: Parse a -> Parse [a]
many' p = Parse $ \s ->
  let (res, s') = (runParse p) s
  in case res of
    (Just r) -> runParse ((r:) <$> many' p) s'
    Nothing  -> (Just [], s)

parseEOF :: Parse ()
parseEOF = Parse $ \case
  [] -> (Just (), [])
  s -> (Nothing, s)

endWithEOF :: Parse a -> Parse a
endWithEOF p = Parse $ \s ->
  let (res, s') = runParse p s
  in if null s' && isJust res
     then (res, s')
     else (Nothing, s)

-- parse rational numbers --
parseDigit :: Parse Integer
parseDigit = Parse $ \case
  '0':cs -> (Just 0, cs)
  '1':cs -> (Just 1, cs)
  '2':cs -> (Just 2, cs)
  '3':cs -> (Just 3, cs)
  '4':cs -> (Just 4, cs)
  '5':cs -> (Just 5, cs)
  '6':cs -> (Just 6, cs)
  '7':cs -> (Just 7, cs)
  '8':cs -> (Just 8, cs)
  '9':cs -> (Just 9, cs)
  s      -> (Nothing, s)

lsToInteger :: [Integer] -> Integer
lsToInteger [] = error "何?"
lsToInteger ls = fst $ foldr (\n (acc, count) -> (n*10^count+acc, count+1)) (0, 0) ls

parseDecimal :: Parse Integer
parseDecimal = lsToInteger <$> some' parseDigit

parseRational :: Parse Rational
parseRational = do
  numerator <- parseDecimal
  parseChr '/'
  denominator <- parseDecimal
  case denominator of
    0 -> fail ""
    _ -> return (numerator % denominator)

-- example NumberOrString --

eitherOr :: String
eitherOr = "\n\n\n123\nabc\n456\ndef\n"

type NumberOrString = Either Integer String

numberorstring :: Parser NumberOrString
numberorstring = (Left <$> integer) <|> (Right <$> some letter)

numberorstrings :: Parser [NumberOrString]
numberorstrings = some $ do
  skipMany (char '\n')
  res <- numberorstring
  skipMany (char '\n')
  return res

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  string "/"
  denominator <- decimal
  case denominator of
    0 -> fail "Yo, don't divide by zero!"
    _ -> return (numerator % denominator)

fraction' :: Parser Rational
fraction' = (%) <$> decimal <*> decimal


decimalorfraction :: Parser (Either Integer Rational)
decimalorfraction = (Right <$> try fraction) <|> (Left <$> decimalOnly)
  where decimalOnly = do
          dec <- decimal
          eof
          return dec

-- ini parser --

inisample :: String
inisample = "; comment\n[section]\nhost=wikipedia.org\nalias=claw"

newtype Header = Header String
  deriving(Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

data Section = Section Header Assignments
  deriving(Eq, Show)
newtype Config = Config (Map Header Assignments)

comment :: Parser ()
comment = char ';' *> skipMany (noneOf "\n") *> skipEOL *> return ()

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = Header <$> parseBracketPair (some letter) <* skipEOL

parseAssignment :: Assignments -> Parser Assignments
parseAssignment as = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEOL
  return $ M.insert name value as

addManyAssignments :: Assignments -> Parser Assignments
addManyAssignments as = option as $
  try (parseAssignment as) >>= addManyAssignments

manyAssignments :: Parser Assignments
manyAssignments = pure M.empty >>= addManyAssignments

someAssignments :: Parser Assignments
someAssignments = pure M.empty >>= parseAssignment >>= addManyAssignments

parseSection :: Parser Section
parseSection = Section <$> parseHeader <*> manyAssignments

skipEOL = skipMany $ char '\n'
