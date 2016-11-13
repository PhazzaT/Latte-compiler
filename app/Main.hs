{-# LANGUAGE FlexibleContexts #-}

module Main where

import CompilePasses


main :: IO ()
main = do
    contents <- getContents
    case compile contents of
        Right t -> putStr t
        Left err -> putStr $ "Error: " ++ err

