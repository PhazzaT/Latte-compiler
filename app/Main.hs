{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import System.IO
import System.Exit

import AST
import CompilePasses
import CompileError


main :: IO ()
main = do
    source <- getContents
    case compile source of
        Right t -> putStr t >> exitSuccess
        Left err -> prettyPrintError source err >> exitFailure


prettyPrintError :: String -> CompileError -> IO ()
prettyPrintError source err@(CompileError _ (PhaseError NoLocInfo s)) =
    hPrint stderr err
prettyPrintError source (CompileError t (PhaseError (LocInfo (ln1, col1) (ln2, col2)) s)) = do
    let ls = lines source
    let (prefix, highlighted, suffix) = if ln1 == ln2
            then triSplit (col1 - 1) (col2 - 1) $ ls !! (ln1 - 1)
            else
                let (_, ls', _) = triSplit (ln1 - 1) ln2 ls
                    ([l1], c, [l2]) = triSplit 1 (length ls' - 1) ls'
                    (pl1, sl1) = splitAt (col1 - 1) l1
                    (pl2, sl2) = splitAt (col2 - 1) l2
                in (pl1, unlines $ [sl1] ++ c ++ [pl2], sl2)

    hPutStr stderr $ if ln1 == ln2
        then "Line " ++ show ln1
        else "Lines " ++ show ln1 ++ "-" ++ show ln2
    hPutStr stderr ":\n"
    hPutStr stderr prefix
    hPutStr stderr redCode
    hPutStr stderr highlighted
    hPutStr stderr whiteCode
    hPutStr stderr suffix
    hPutStr stderr "\n"
    hPrint stderr $ CompileError t $ PhaseError NoLocInfo s


triSplit :: Int -> Int -> [a] -> ([a], [a], [a])
triSplit i1 i2 l =
    let (p, l') = splitAt i1 l
        (c, s) = splitAt (i2 - i1) l'
    in (p, c, s)


redCode :: String
redCode = "\ESC[31m"


whiteCode :: String
whiteCode = "\ESC[37m"

