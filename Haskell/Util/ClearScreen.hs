module Haskell.Util.ClearScreen where

import System.Info (os)
import System.Process (system)

clearScreen :: IO ()
clearScreen = do
    case os of
        "linux" -> do
            _ <- system "clear"
            return ()
        "mingw32" -> do
            _ <- system "cls"
            return ()
        _ -> return ()