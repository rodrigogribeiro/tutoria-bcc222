module Parser ( Parser
              , runParser
              , symbol
              , token
              , sat
              , digitChar
              , spaces
              , succeed
              , failure
              , (<|>)
              , option
              , many
              , many1
              , greedy
              , greedy1
              , pack
              , parenthesized
              , endBy
              , chainl
              , chainr
              , listOf
              , digit
              , natural
              , identifier) where

import Data.Char


-- representation of parsers

newtype Parser s a
  = Parser {
    runParser :: [s] -> [(a,[s])]
    }

-- simple parsers

symbol :: Eq s => s -> Parser s s
symbol s
  = Parser (\ inp ->
       case inp of
         [] -> []
         (x : xs) -> if x == s
                     then [(x,xs)]
                     else [])

token :: Eq s => [s] -> Parser s [s]
token s
  = Parser (\ inp ->
        if s == (take n inp)
        then [(s, drop n inp)]
        else [])
    where
      n = length s

sat :: (s -> Bool) -> Parser s s
sat p = Parser (\ inp ->
        case inp of
          [] -> []
          (x : xs) ->
            if p x
            then [(x,xs)]
            else [])

digitChar :: Parser Char Char
digitChar = sat isDigit

spaces :: Parser Char String
spaces = greedy (sat isSpace)

succeed :: a -> Parser s a
succeed v = Parser (\ inp -> [(v,inp)])

failure :: Parser s a
failure = Parser (\ _ -> [])


-- instances for Parser

instance Functor (Parser s) where
   fmap f (Parser p)
      = Parser (\ inp ->
                  [(f x, xs) | (x,xs) <- p inp])

instance Applicative (Parser s) where
   pure = succeed
   (Parser p) <*> (Parser q)
     = Parser (\ inp -> [(f x, xs) | (f, ys) <- p inp
                                   , (x, xs) <- q ys])

instance Monad (Parser s) where
  return = pure
  (Parser p) >>= f
    = Parser (\ inp ->
                let r = p inp
                in concatMap (\(x,s') -> runParser (f x) s') r)

-- combinators

infixr 4 <|>

(<|>) :: Parser s a -> Parser s a -> Parser s a
(Parser p) <|> (Parser q)
   = Parser (\ inp -> p inp ++
                      q inp)

option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

many :: Parser s a -> Parser s [a]
many p = ((:) <$> p <*> many p) <|> succeed []

many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> many p

first :: Parser s a -> Parser s a
first (Parser p)
   = Parser (\ inp -> let r = p inp
                      in if null r then []
                         else [head r])

greedy :: Parser s a -> Parser s [a]
greedy = first . many

greedy1 :: Parser s a -> Parser s [a]
greedy1 = first . many1

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p sep
   = (:) <$> p <*> many ((\ _ y -> y) <$> sep <*> p)

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p q r = (\ _ x _ -> x) <$> p <*> q <*> r

parenthesized :: Parser Char a -> Parser Char a
parenthesized p = pack (symbol '(') p (symbol ')')

endBy :: Parser s a -> Parser s b -> Parser s [a]
endBy p sep = greedy ((\ x _ -> x) <$> p <*> sep)

chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po
   = h <$> many (j <$> pe <*> po) <*> pe
     where
       j x op = op x
       h fs x = foldr ($) x fs

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po
   = h <$> pe <*> many (j <$> po <*> pe)
     where
       j op x = \ y -> op y x
       h x fs = foldl (flip ($)) x fs

-- examples

digit :: Parser Char Int
digit = f <$> digitChar
        where
          f c = ord c - ord '0'

natural :: Parser Char Int
natural = foldl f 0 <$> many1 digit
     where
       f ac d = ac * 10 + d


identifier :: Parser Char String
identifier
   = (:) <$> letter <*> greedy (sat isAlphaNum)
     where
       letter = sat isLetter
