module Stacks (FishSticks
              , pop
              , push
              , pushHex
              , pushChar
              , getTop
              ) where

import qualified Data.Map as M
import Data.Char (ord)
import Fraction

-- |A list of pairs of a stack and a register. Values are pairs of Integers
-- representing a fraction. FishSticks is one letter off from FishStacks.
type FishSticks = [( [Fraction], Maybe Fraction )]

-- |Pop n values off the top stack
pop :: Int -> FishSticks -> FishSticks
pop n ((xs, r) : stacks) = (drop n xs, r) : stacks

-- |Push a fraction onto the stack
push :: Fraction -> FishSticks -> FishSticks
push n ((xs, r) : stacks) = (n : xs, r) : stacks

-- |Push a hex value onto the stack
pushHex :: Char -> FishSticks -> FishSticks
pushHex c ((xs, r) : stacks) =
    if c == '0'
        then ((0, 0)        : xs, r) : stacks
        else ((getHex c, 1) : xs, r) : stacks

-- |Convert a hex literal to an Integer
getHex :: Char -> Integer
getHex c =
  let hexMap = M.fromList [('0',0), ('1',1), ('2',2), ('3',3), ('4',4), ('5',5), ('6',6), ('7',7)
                          ,('8',8), ('9',9), ('a',10),('b',11),('c',12),('d',13),('e',14),('f',15)]
  in case M.lookup c hexMap of
       Just n  -> n
       Nothing -> 1

-- |Push a character onto the stack
pushChar :: Char -> FishSticks -> FishSticks
pushChar c ((xs, r) : stacks) = ((fromIntegral $ ord c, 1) : xs, r) : stacks

-- |Get the top value on the top stack
getTop :: FishSticks -> Fraction
getTop ((x:xs, r) : stacks) = x
getTop (([], r)   : stacks) = error "Something smells fishy..."
