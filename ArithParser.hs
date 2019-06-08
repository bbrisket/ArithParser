module ArithParser where

import Data.List
import Data.Char
import Debug.Trace
import Prelude hiding (gcd)

data ArithExp
  = Frac (Integer, Integer)     -- (a,b) represents a / b
  | Plus ArithExp ArithExp
  | Mult ArithExp ArithExp
  | Div  ArithExp ArithExp
  deriving (Show)

gcd :: (Integer, Integer) -> Integer
gcd (a, 0) = a
gcd (a, b) = gcd (b, (a `mod` b))

reduce :: (Integer, Integer) -> (Integer, Integer)    -- simplifying fractions
reduce (a,b)
  | b < 0     = reduce (-a, -b)
  | otherwise = (a `quot` (gcd (a,b)), b `quot` (gcd (a,b)))


eval :: ArithExp -> (Integer, Integer)   -- mechanics for fractional arithmetic
eval x =
  case x of
    Frac   (a, b) -> (a, b)
    Plus   a1 b1 -> reduce (m1 * q1 + n1 * p1, n1 * q1)
      where (m1,n1) = eval a1
            (p1,q1) = eval b1
    Mult   a2 b2 -> reduce (m2 * p2, n2 * q2)
      where (m2,n2) = eval a2
            (p2,q2) = eval b2
    Div    a3 b3 -> reduce (m3 * q3, n3 * p3)
      where (m3,n3) = eval a3
            (p3,q3) = eval b3

data Token
  = NumToken (Integer, Integer)
  | PlusToken
  | MultToken
  | DivToken
  | LParenToken
  | RParenToken
  deriving (Show, Eq) --testing tokenize in isolation

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':rest) = PlusToken   : (tokenize rest)
tokenize ('*':rest) = MultToken   : (tokenize rest)
tokenize ('/':rest) = DivToken    : (tokenize rest)
tokenize ('(':rest) = LParenToken : (tokenize rest)
tokenize (')':rest) = RParenToken : (tokenize rest)
tokenize (' ':rest) = (tokenize rest)
-- Note: integer n is read in as (n/1)
tokenize ('-':str)  = NumToken (-(read num :: Integer),1) : (tokenize rest) where --handles negatives
  (num,rest) = break (\chr -> not (isDigit chr)) str
tokenize str = NumToken (read num :: Integer,1) : (tokenize rest) where
  (num,rest) = break (\chr -> not (isDigit chr)) str

parse :: [Token] -> ArithExp
parse ((NumToken (a,b)):[]) = Frac (a,b)
parse tokList
-- When the first ')' is read, a corresponding '(' with an evaluatable expression
-- inside must have already been read in. The first line of code evaluates the
-- expression immediately and gets rid of those pesky parentheses.
  | (RParenToken `elem` tokList) = parse (((reverse (tail d)) ++ [NumToken (eval (parse (reverse c)))]) ++ (tail b))
  | (PlusToken   `elem` tokList) = Plus (parse first1) (parse . tail $ second1)
  | (MultToken   `elem` tokList) = Mult (parse first2) (parse . tail $ second2)
-- The expression 1/2/3 should be calculated as (1/2)/3 instead of 1/(2/3), so
-- the rightmost division should be executed first, and so on going leftward.
  | (DivToken    `elem` tokList) = Div  (parse . reverse . tail $ first3) (parse . reverse $ second3)
  | otherwise = Frac (0,1)    --should never be reached
    where (first1,second1) = break (PlusToken ==) tokList -- finds location of token operators
          (first2,second2) = break (MultToken ==) tokList
          (second3,first3) = break (DivToken  ==) (reverse tokList)
          (a, b) = break (RParenToken ==) tokList
          (c, d) = break (LParenToken ==) (reverse a)
