{-# LANGUAGE RecursiveDo                #-}

module Main where

a :: String
a = "hello"

test :: IO ()
test = do
    rec putStrLn a
    let a = 3 :: Int
    print a
