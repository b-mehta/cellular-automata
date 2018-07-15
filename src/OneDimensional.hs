{-# LANGUAGE DeriveFunctor #-}
module OneDimensional (Rule, Direction(..), initial, setInitial, run, trunc)
  where

import Control.Comonad                       (Comonad, extract, duplicate, extend)
import Data.Word                             (Word8)
import Data.Map.Strict                       (Map, (!))
import Data.Bits                             (testBit)
import qualified Data.Map.Strict as M

data U a = U [a] a [a] deriving Functor

left, right :: U a -> U a
left (U l x r) = U (tail l) (head l) (x:r)
right (U l x r) = U (x:l) (head r) (tail r)

instance Comonad U where
    extract (U _ x _) = x
    duplicate x = U (tail $ iterate left x) x (tail $ iterate right x)

data Direction = L | C | R
  deriving Read

trunc :: Direction -> Int -> U a -> [a]
trunc R n (U _ x r) = x:(take (n-1) r)
trunc L n (U l x _) = reverse $ x:(take (n-1) l)
trunc C n (U l x r)
  | odd n     = reverse (take m l) ++ (x:take m r)
  | otherwise = reverse (take m l) ++ (x:take (m+1) r)
  where m = (n-1) `div` 2

type Rule = Word8

memo :: Rule -> Map (Bool, Bool, Bool) Bool
memo r = M.fromList [(binary n, testBit r n) | n <- [0..7]]
  where binary :: Int -> (Bool, Bool, Bool)
        binary n = (testBit n 2, testBit n 1, testBit n 0)

oneStep :: Rule -> U Bool -> Bool
oneStep rul = \(U l x r) -> memo rul ! (head l,x,head r)

step :: Rule -> U Bool -> U Bool
step = extend . oneStep

run :: Rule -> U Bool -> [U Bool]
run = iterate . step

initial :: U Bool
initial = U (repeat False) True (repeat False)

setInitial :: ([Bool], [Bool]) -> U Bool
setInitial (y,x) = U (y ++ repeat False) (head x) (tail x ++ repeat False)
