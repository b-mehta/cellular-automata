{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Cellular
  where

import qualified Data.Vector.Storable as V   (fromList, force, concat)
import Control.Arrow                         ((***))
import Data.List                             (partition)
import System.Random                         (randoms, getStdGen, RandomGen)
import Codec.Picture.Types                   (DynamicImage(ImageY8))
import Codec.Picture                         (savePngImage, Image(Image), Pixel8)
import Options.Applicative                   (fullDesc, progDesc, helper, header, info, execParser)

import OneDimensional                        (Direction(..), initial, setInitial, run, trunc)
import ArgParse                              (Info(..), inform)

-- Cells to pixels
toPixel :: Bool -> Pixel8
toPixel = \case True -> 0
                False -> 255

-- Universe to image
makeImage :: Int -> Int -> [[Bool]] -> Image Pixel8
makeImage m n = Image m n . V.concat . map (V.force . V.fromList) . map (map toPixel)

----------------------------------------------------------------------------------------------------

-- Create the required image
cellular :: Info -> IO ()
cellular Info{..} = do
  (t,first) <- case start of
                     Left p -> do
                                g <- getStdGen
                                return (R, setInitial . splitter . bernoullis p $ g)
                     Right dir -> return (dir, initial)
  let evolutions = run rule first
  let region = map (trunc t width) . take steps $ evolutions
  savePngImage output . ImageY8 . makeImage width steps $ region

-- An infinite bernoulli sample
bernoullis :: RandomGen g => Double -> g -> [Bool]
bernoullis p = map (<p) . randoms

-- Split a list into two parts (used here to convert a singly infinite stream to doubly)
splitter :: [a] -> ([a], [a])
splitter = (map snd *** map snd) . partition (even . fst) . zip [0..]

----------------------------------------------------------------------------------------------------

main :: IO ()
main = cellular =<< execParser opts
  where
    opts = info (helper <*> inform)
      (  fullDesc
      <> progDesc "Evaluate some steps of a cellular automaton"
      <> header "One-dimensional cellular automata")
