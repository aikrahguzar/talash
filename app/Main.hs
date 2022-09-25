module Main where

import qualified Talash.Brick as B
import qualified Talash.Piped as P
import qualified Talash.SimpleSearcher as S
import Prelude
import System.Environment (getArgs)

run :: [String] -> IO ()
run ("piped":xs) = P.run' xs
run ("tui"  :xs) = B.run' xs
run ["test"]     = S.simpleSearcherTest
run  _           = putStrLn usageString

usageString :: String
usageString =    "talash is the demo program for the talash library. It has two set of subcommand:\n"
              <> "talash tui : for the terminal user interface, use 'talash tui help' for details.\n"
              <> "talash piped : for the piped interface, use 'talash piped help' for details.\n"

main :: IO ()
main = run =<< getArgs
