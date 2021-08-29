module Main where

import Prelude (IO)
import qualified Prelude

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = Prelude.putStrLn "Running"
