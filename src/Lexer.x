-- Hand-written lexer for the Latte language
-- TODO: Consider unicode parsing
{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lexer where

import Control.Applicative
import Control.Monad.Except
import Data.Generics
import Data.Monoid

import AST
import CompileError
}

%wrapper "posn"

tokens :-
    $white+ ;

    -- Comments
    "#".*   ;
    "//".*  ;
    "/*" ([. \n # \*] | \* [. \n # \/])* "*/" ;

    -- Keywords
    "if"        { simpleTok TokIf }
    "else"      { simpleTok TokElse }
    "while"     { simpleTok TokWhile }
    "return"    { simpleTok TokReturn }
    "new"       { simpleTok TokNew }
    "for"       { simpleTok TokFor }
    "class"     { simpleTok TokClass }
    "null"      { simpleTok TokNull }

    -- Literals
    "true"                               { simpleTok $ TokBoolLiteral True }
    "false"                              { simpleTok $ TokBoolLiteral False }
    [0-9]+                               { stringTok $ TokIntLiteral . read }
    \" ([$printable # \"] | (\\ \"))* \" { actionTok (\s l -> fmap TokStrLiteral $ decodeEscapeCodes s l) }
    [a-zA-Z_] [a-zA-Z0-9_]*              { stringTok TokIdentifier }

    -- Symbols
    ";"         { simpleTok TokSemicolon }
    ":"         { simpleTok TokColon }
    ","         { simpleTok TokComma }
    "."         { simpleTok TokDot }
    "("         { simpleTok TokOpenParen }
    ")"         { simpleTok TokCloseParen }
    "["         { simpleTok TokOpenSquare }
    "]"         { simpleTok TokCloseSquare }
    "{"         { simpleTok TokOpenCurly }
    "}"         { simpleTok TokCloseCurly }

    -- Operators
    "!"         { simpleTok TokNot }
    "||"        { simpleTok TokOr }
    "&&"        { simpleTok TokAnd }
    "++"        { simpleTok TokIncrement }
    "--"        { simpleTok TokDecrement }
    "+"         { simpleTok TokPlus }
    "-"         { simpleTok TokMinus }
    "*"         { simpleTok TokMultiply }
    "/"         { simpleTok TokDivide }
    "%"         { simpleTok TokModulo }
    "<"         { simpleTok TokLess }
    "<="        { simpleTok TokLessEqual }
    ">"         { simpleTok TokGreater }
    ">="        { simpleTok TokGreaterEqual }
    "=="        { simpleTok TokEqual }
    "!="        { simpleTok TokNotEqual }
    "="         { simpleTok TokAssign }

{
data TokenData = TokIf
               | TokElse
               | TokWhile
               | TokReturn
               | TokNew
               | TokFor
               | TokClass
               | TokNull
               | TokIdentifier String
               | TokIntLiteral Integer
               | TokStrLiteral String
               | TokBoolLiteral Bool
               | TokSemicolon
               | TokColon
               | TokComma
               | TokDot
               | TokOpenParen
               | TokCloseParen
               | TokOpenSquare
               | TokCloseSquare
               | TokOpenCurly
               | TokCloseCurly
               | TokNot
               | TokOr
               | TokAnd
               | TokIncrement
               | TokDecrement
               | TokPlus
               | TokMinus
               | TokMultiply
               | TokDivide
               | TokModulo
               | TokLess
               | TokLessEqual
               | TokGreater
               | TokGreaterEqual
               | TokEqual
               | TokNotEqual
               | TokAssign
               deriving (Eq, Show)

data Token = Token { tokenData     :: !TokenData
                   , tokenLoc      :: !LocInfo }
                   deriving (Eq)

instance Show Token where
    show t = "(T " ++ show (tokenData t) ++ " " ++ show (tokenLoc t) ++ ")"


type Alex = Either PhaseError


alexPnToLoc :: AlexPosn -> String -> LocInfo
alexPnToLoc (AlexPn _ ln col) s = LocInfo (ln, col) (ln, col + length s)


simpleTok :: TokenData -> AlexPosn -> String -> Alex Token
simpleTok td p s = return $ Token td $ alexPnToLoc p s


stringTok :: (String -> TokenData) -> AlexPosn -> String -> Alex Token
stringTok f p s = return $ Token (f s) $ alexPnToLoc p s


actionTok :: (String -> LocInfo -> Alex TokenData) -> AlexPosn -> String -> Alex Token
actionTok m p s = Token <$> m s loc <*> pure loc
    where loc = alexPnToLoc p s


tokenize :: String -> Alex [Token]
tokenize s = go (alexStartPos, '\n', [], s) []
    where
        go input@(pos, _, _, str) toks =
            case alexScan input 0 of
                AlexToken input' len m               -> m pos (take len str) >>= \act -> go input' (act : toks)
                AlexSkip input' _                    -> go input' toks
                AlexEOF                              -> return $ reverse toks
                AlexError (pn, _, _, _) ->
                    throwError $ PhaseError (alexPnToLoc pn str) "Lexical error"


decodeEscapeCodes :: String -> LocInfo -> Alex String
decodeEscapeCodes s (LocInfo (ln, col) _) = go (Endo id) s 1
    where
        go :: Endo String -> String -> Int -> Alex String
        go es ('\\':c:cs) i =
            case lookup c escapes of
                Just x -> go (es <> Endo (x:)) cs (i + 2)
                Nothing -> throwError $ PhaseError (loc i 2)
                                        $ "Unrecognized escape code: \\" ++ [c]
        go _ "\\" i = throwError $ PhaseError (loc i 1) "???"
        go es (c:cs) i = go (es <> Endo (c:)) cs (i + 1)
        go es "" _ = return $ appEndo es ""

        escapes :: [(Char, Char)]
        escapes = [ ('n', '\n')
                  , ('t', '\t')
                  , ('\\', '\\')
                  ]
        loc i len = LocInfo (ln, col + i) (ln, col + i + len)
}

