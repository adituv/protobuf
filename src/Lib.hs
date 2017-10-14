module Lib
    ( someFunc
    ) where

import TestProto(someString)

someFunc :: IO ()
someFunc = putStrLn someString
