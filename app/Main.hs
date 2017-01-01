{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.Exit

import CompilePasses
import CompileError


main :: IO ()
main = do
    contents <- getContents
    case compile contents of
        Right t -> putStr t >> exitSuccess
        Left err -> hPrint stderr err >> exitFailure

