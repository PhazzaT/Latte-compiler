-- Hand-written lexer for the Latte language
-- TODO: Consider unicode parsing
{
{-# LANGUAGE FlexibleContexts #-}
module Lexer where

import Control.Applicative
import Control.Monad.Except
import Data.Monoid
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
    \" ([$printable # \"] | (\\ \"))* \" { actionTok (fmap TokStrLiteral . decodeEscapeCodes . init . tail) }
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
                   , tokenPosition :: !AlexPosn }
                   deriving (Eq)

instance Show Token where
    show t = "(T " ++ show (tokenData t) ++ " " ++ show (tokenPosition t) ++ ")"


type Alex = Either String


simpleTok :: TokenData -> AlexPosn -> String -> Alex Token
simpleTok td p _ = return $ Token td p


stringTok :: (String -> TokenData) -> AlexPosn -> String -> Alex Token
stringTok f p s = return $ Token (f s) p


actionTok :: (String -> Alex TokenData) -> AlexPosn -> String -> Alex Token
actionTok m p s = Token <$> m s <*> pure p


tokenize :: String -> Alex [Token]
tokenize s = go (alexStartPos, '\n', [], s) []
    where
        go input@(pos, _, _, str) toks =
            case alexScan input 0 of
                AlexToken input' len m               -> m pos (take len str) >>= \act -> go input' (act : toks)
                AlexSkip input' _                    -> go input' toks
                AlexEOF                              -> return $ reverse toks
                AlexError (AlexPn _ ln col, _, _, _) ->
                    throwError $ "Lexical error at (" ++ show ln ++ ", " ++ show col ++ ")"


decodeEscapeCodes :: String -> Alex String
decodeEscapeCodes s = go (Endo id) s
    where
        go :: Endo String -> String -> Alex String
        go es ('\\':c:cs) =
            case lookup c escapes of
                Just x -> go (es <> Endo (x:)) cs
                Nothing -> throwError $ "Unrecognized escape code: \\" ++ [c]
        go _ "\\" = throwError "???"
        go es (c:cs) = go (es <> Endo (c:)) cs
        go es "" = return $ appEndo es ""

        escapes :: [(Char, Char)]
        escapes = [ ('n', '\n')
                  , ('t', '\t')
                  , ('\\', '\\')
                  ]
}

