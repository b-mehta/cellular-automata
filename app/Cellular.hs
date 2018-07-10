{-# LANGUAGE DeriveFunctor, RecordWildCards #-}
module Cellular
  where
import Control.Comonad                       (Comonad, extract, duplicate, extend)
import Control.Arrow                         ((&&&),(***))
import Data.Bits                             (testBit)
import Data.Word                             (Word8)
import Data.List                             (intercalate)
import System.Random                         (randoms, split, getStdGen)
import Data.Maybe (isJust)
import Debug.Trace
import Control.Monad                         ()
import Codec.Picture.Types
import Codec.Picture
import Options.Applicative
import Data.Map.Strict                       (Map, (!))
import qualified Data.Map.Strict as M

data U a = U [a] a [a] deriving Functor

left :: U a -> U a
left (U l x r) = U (tail l) (head l) (x:r)

right :: U a -> U a
right (U l x r) = U (x:l) (head r) (tail r)

instance Comonad U where
    extract (U _ x _) = x
    duplicate x = U (tail $ iterate left x) x (tail $ iterate right x)

truncateWorld, truncateLeft, truncateRight :: Int -> U a -> [a]
truncateRight n (U _ x r) = x:(take (n-1) r)
truncateLeft n (U l x _) = reverse $ x:(take (n-1) l)
truncateWorld n (U l x r)
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

makeImage :: Int -> Int -> [[Bool]] -> IO (Image Pixel8)
makeImage m n world = do
  img <- createMutableImage m n 255
  sequence_ [writePixel img x y 0 | (xs,y) <- zip world [0..(n-1)], (t,x) <- zip xs [0..(m-1)], t]
  unsafeFreezeImage img

data Info = Info
  { rule :: Rule
  , steps :: Int
  , width :: Int
  , single :: Maybe Direction
  , output :: String
  }

data Direction = L | C | R
  deriving Read

inform :: Parser Info
inform = Info
      <$> option auto
          (  long "rule"
          <> help "Use rule RULE"
          <> metavar "RULE"
          )
      <*> option auto
          (  long "iter"
          <> help "Number of iterations to take"
          <> metavar "INT"
          )
      <*> option auto
          (  long "width"
          <> help "Approximate width of output"
          <> metavar "INT"
          )
      <*> optional (option auto
          (  short 's'
          <> help "Use a single bit as the start row and pick direction (otherwise random)"
          ))
      <*> strOption
          (  long "output" <> short 'o'
          <> value "output.png"
          <> metavar "OUT"
          <> help "Write output to OUT"
          )

cellular :: Info -> IO ()
cellular (Info{..}) = do
  first <- if isJust single
             then return initial
             else getStdGen >>= return . setInitial . (randoms *** randoms) . split
  let trunc = case single of
            Just L -> truncateLeft
            Just R -> truncateRight
            _      -> truncateWorld
  let ans = map (trunc width) . take steps . run rule $ first
  img <- makeImage width steps ans
  savePngImage output (ImageY8 img)

main :: IO ()
main = cellular =<< execParser opts
  where
    opts = info (inform <**> helper)
      (  fullDesc
      <> progDesc "Evaluate some steps of a cellular automaton"
      <> header "One-dimensional cellular automata")
