module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  hints <- hlint $ ["src"
                   , "--cpp-define=HLINT"
                   , "--cpp-ansi"
                   , "--ignore=Use camelCase"] ++ args
  unless (null hints) exitFailure
