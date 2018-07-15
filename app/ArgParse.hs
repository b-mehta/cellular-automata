module ArgParse (Info(..), inform)
  where

import Options.Applicative                   (Parser, value, metavar, help, long, short, strOption, auto, option)
import Control.Applicative                   ((<|>))
import OneDimensional                        (Rule, Direction)

data Info = Info
  { rule :: Rule
  , steps :: Int
  , width :: Int
  , start :: Either Double Direction
  , output :: String
  }

inform :: Parser Info
inform = Info
      <$> option auto
          (  long "rule" <> short 'r'
          <> help "Use rule RULE"
          <> metavar "RULE")
      <*> option auto
          (  long "iter"
          <> help "Number of iterations to take"
          <> metavar "INT")
      <*> option auto
          (  long "width"
          <> help "Width of output"
          <> metavar "INT")
      <*> (Left <$> option auto
          (  short 'p'
          <> help "Use a random first row and set its probability"
          <> metavar "PROB")
      <|> (Right <$> option auto
          (  short 's'
          <> help "Use a single bit first row and choose direction"
          <> metavar "L/C/R")))
      <*> strOption
          (  long "output" <> short 'o'
          <> value "output.png"
          <> metavar "OUT"
          <> help "Write output to OUT")
