-- Hand-written grammar for Latte language
{
module Parser(parse) where

import Control.Monad.Except
import Control.Monad.State

import AST
import Lexer
}

-- A word on block numbering convention:
-- The global scope has block ID 0. The ParseMonad keeps track of the next
-- number to assign to a block (starting with 1). In my opinion it would be
-- more natural to assign block IDs in preorder fashion, but unfortunately
-- Happy performs monadic actions for subexpressions BEFORE action of the
-- enclosing block - so every block gets an ID larger than any of its inner
-- blocks. For example:
--     void foo() {
--         int x;
--         { string y; }
--     }
-- Symbols are defined in following blocks:
--     foo - 0
--     x   - 2
--     y   - 1
-- TODO: Try to change it?

-- TODO: Try to somehow extract token positions

%name parseInner
%tokentype { Token }
%monad { ParseMonad }
%error { parseError }


%token
    -- Keywords
    if          { Token TokIf _ }
    else        { Token TokElse _ }
    while       { Token TokWhile _ }
    return      { Token TokReturn _ }

    -- Literals
    identifier      { Token (TokIdentifier $$) _ }
    intLiteral      { Token (TokIntLiteral $$) _ }
    stringLiteral   { Token (TokStrLiteral $$) _ }
    boolLiteral     { Token (TokBoolLiteral $$) _ }

    -- Symbols
    ';'             { Token TokSemicolon _ }
    ','             { Token TokComma _ }
    '('             { Token TokOpenParen _ }
    ')'             { Token TokCloseParen _ }
    '['             { Token TokOpenSquare _ }
    ']'             { Token TokCloseSquare _ }
    '{'             { Token TokOpenCurly _ }
    '}'             { Token TokCloseCurly _ }

    -- Operators
    '!'             { Token TokNot _ }
    '||'            { Token TokOr _ }
    '&&'            { Token TokAnd _ }
    '++'            { Token TokIncrement _ }
    '--'            { Token TokDecrement _ }
    '+'             { Token TokPlus _ }
    '-'             { Token TokMinus _ }
    '*'             { Token TokMultiply _ }
    '/'             { Token TokDivide _ }
    '%'             { Token TokModulo _ }
    '<'             { Token TokLess _ }
    '<='            { Token TokLessEqual _ }
    '>'             { Token TokGreater _ }
    '>='            { Token TokGreaterEqual _ }
    '=='            { Token TokEqual _ }
    '!='            { Token TokNotEqual _ }
    '='             { Token TokAssign _ }


%%


-- Programs --

Program :: { Program }
Program : list(FnDef) { Program $1 }

FnDef :: { FnDef }
FnDef : Type identifier '(' listSep(Arg, ',') ')' Block { FnDef $1 $2 $4 $6 }

Arg :: { Arg }
Arg : Type identifier { Arg $1 $2 }


-- Statements --

Block :: { Stmt }
Block : '{' list(Stmt) '}' {% state $ \i -> (Block i $2, i + 1) }

Stmt :: { Stmt }
Stmt : Type listSepNEmpty(Item, ',') ';' { Decl $1 $2 }
     | Stmt1                             { $1 }

Stmt1 :: { Stmt }
Stmt1 : ';'                              { Empty }
      | identifier '=' Expr ';'          { Assign $1 $3 }
      | identifier '++' ';'              { Assign $1 (EAdd (EVar $1) Plus (EIntLiteral 1)) }
      | identifier '--' ';'              { Assign $1 (EAdd (EVar $1) Minus (EIntLiteral 1)) }
      | return Expr ';'                  { Return $2 }
      | return ';'                       { VReturn }
      | if '(' Expr ')' Stmt1 else Stmt1 { If $3 $5 $7 }
      | if '(' Expr ')' Stmt1            { If $3 $5 Empty }
      | while '(' Expr ')' Stmt1         { While $3 $5 }
      | Expr ';'                         { SExpr $1 }
      | Block                            { $1 }

Item :: { Item }
Item : identifier          { ItemDeclDefault $1 }
     | identifier '=' Expr { ItemDecl $1 $3 }


-- Expressions --

Expr :: { Expr }
Expr : Expr1 '||' Expr { ELogic $1 Or $3 }
     | Expr1           { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { ELogic $1 And $3 }
      | Expr2            { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { ERel $1 $2 $3 }
      | Expr3             { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { EAdd $1 $2 $3 }
      | Expr4             { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { EMul $1 $2 $3 }
      | Expr5             { $1 }
Expr5 :: { Expr }
Expr5 : '!' Expr6 { Not $2 }
      | '-' Expr6 { Neg $2 }
      | Expr6     { $1 }
Expr6 :: { Expr }
Expr6 : stringLiteral                         { EString $1 }
      | identifier '(' listSep(Expr, ',') ')' { EApp $1 $3 }
      | boolLiteral                           { EBoolLiteral $1 }
      | intLiteral                            { EIntLiteral $1 }
      | identifier                            { EVar $1 }
      | '(' Expr ')'                          { $2 }


-- Operators --

AddOp :: { AddOp }
AddOp : '+' { Plus }
      | '-' { Minus }

MulOp :: { MulOp }
MulOp : '*' { Multiply }
      | '/' { Divide }
      | '%' { Modulo }

RelOp :: { RelOp }
RelOp : '<'  { Less }
      | '<=' { LessEqual }
      | '>'  { Greater }
      | '>=' { GreaterEqual }
      | '==' { Equal }
      | '!=' { NotEqual }


-- Types --

Type :: { Type }
Type : identifier { TNamed $1 }


-- Auxiliary parametrized productions --

listNEmpty(x) : x listNEmpty(x) { $1 : $2 }
              | x               { [$1] }

list(x) : listNEmpty(x) { $1 }
        |               { [] }

listSepNEmpty(x, s) : x s listSepNEmpty(x, s) { $1 : $3 }
                    | x                       { [$1] }

listSep(x, s) : listSepNEmpty(x, s) { $1 }
              |                     { [] }

listSufNEmpty(x, s) : x s listSufNEmpty(x, s) { $1 : $3 }
                    | x s                     { [$1] }

listSuf(x, s) : listSufNEmpty(x, s) { $1 }
              |                     { [] }


{
type ParseMonad = StateT Integer (Either String)

parseError :: [Token] -> ParseMonad a
parseError (t@(Token _ (AlexPn _ ln col)):_) =
    throwError $ "Parse error at (" ++ show ln ++ ", " ++ show col ++ "), "
              ++ "unexpected token " ++ show t
parseError [] = throwError "Parse error - unexpected end of file"

parse :: [Token] -> Either String Program
parse s = evalStateT (parseInner s) (globalScopeID + 1)
}

