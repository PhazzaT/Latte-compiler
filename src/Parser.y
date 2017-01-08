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
    new         { Token TokNew _ }
    for         { Token TokFor _ }
    class       { Token TokClass _ }
    null        { Token TokNull _ }

    -- Literals
    identifier      { Token (TokIdentifier $$) _ }
    intLiteral      { Token (TokIntLiteral $$) _ }
    stringLiteral   { Token (TokStrLiteral $$) _ }
    boolLiteral     { Token (TokBoolLiteral $$) _ }

    -- Symbols
    ';'             { Token TokSemicolon _ }
    ':'             { Token TokColon _ }
    ','             { Token TokComma _ }
    '.'             { Token TokDot _ }
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
Program : list(TopLevel) { programFromTLs $1 }

TopLevel :: { TopLevel }
TopLevel : ClassDef { TLClass $1 }
         | FnDef    { TLFn $1 }

ClassDef :: { ClassDef }
ClassDef : class Identifier '{' list(MemberDef) '}' { ClassDef $2 $ concat $4 }

MemberDef :: { [(String, Type)] }
MemberDef : Type listSepNEmpty(Identifier, ',') ';' { map (\x -> (x, $1)) $2 }

FnDef :: { FnDef }
FnDef : Type Identifier '(' listSep(Arg, ',') ')' Block { FnDef $1 $2 $4 $6 }

Arg :: { Arg }
Arg : Type Identifier { Arg $1 $2 }


-- Statements --

Block :: { Stmt }
Block : '{' list(Stmt) '}' {% makeBlock $2 }

Stmt :: { Stmt }
Stmt : Type listSepNEmpty(Item, ',') ';' { Decl $1 $2 }
     | Stmt1                             { $1 }

Stmt1 :: { Stmt }
Stmt1 : ';'                              { Empty }
      | Expr '=' Expr ';'                { Assign $1 $3 }
      | Expr '++' ';'                    { Incr $1 }
      | Expr '--' ';'                    { Decr $1 }
      | return Expr ';'                  { Return $2 }
      | return ';'                       { VReturn }
      | if '(' Expr ')' Stmt1 else Stmt1 { If $3 $5 $7 }
      | if '(' Expr ')' Stmt1            { If $3 $5 Empty }
      | while '(' Expr ')' Stmt1         { While $3 $5 }
      | for '(' Type Identifier ':' Expr ')' Stmt1
                                         {% desugarFor $3 $4 $6 $8 }
      | Expr ';'                         { SExpr $1 }
      | Block                            { $1 }

Item :: { Item }
Item : Identifier          { Item $1 Nothing }
     | Identifier '=' Expr { Item $1 (Just $3) }


-- Expressions --

-- Based on official Java syntax
ExprPrimary :: { Expr }
ExprPrimary : ExprPrimaryNoNewArray { $1 }
            | ExprArrayCreation     { $1 }
            | ExprObjectCreation    { $1 }

ExprPrimaryNoNewArray :: { Expr }
ExprPrimaryNoNewArray : ExprLiteral     { $1 }
                      | ExprArrayAccess { $1 }
                      | ExprFieldAccess { $1 }
                      | '(' Expr ')'    { $2 }

ExprLiteral :: { Expr }
ExprLiteral : stringLiteral { EString $1 }
            | boolLiteral   { EBoolLiteral $1 }
            | intLiteral    { EIntLiteral $1 }
            | null          { ENull }

ExprArrayAccess :: { Expr }
ExprArrayAccess : Identifier '[' Expr ']'            { EApp "[]" [EVar $1, $3] }
                | ExprPrimaryNoNewArray '[' Expr ']' { EApp "[]" [$1, $3] }

ExprFieldAccess :: { Expr }
ExprFieldAccess : Expr6 '.' Identifier { EApp ('.' : $3) [$1] }

ExprArrayCreation :: { Expr }
ExprArrayCreation : new Identifier '[' Expr ']' ArrayMarks { ENew (timesArray ($6 + 1) (TNamed $2)) [$4] }

ExprObjectCreation :: { Expr }
ExprObjectCreation : new Identifier { ENew (TNamed $2) [] }

Expr :: { Expr }
Expr : Expr1 '||' Expr { EApp "||" [$1, $3] }
     | Expr1           { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { EApp "&&" [$1, $3] }
      | Expr2            { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { EApp (show $2) [$1, $3] }
      | Expr3             { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { EApp (show $2) [$1, $3] }
      | Expr4             { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { EApp (show $2) [$1, $3] }
      | Expr5             { $1 }
Expr5 :: { Expr }
Expr5 : '!' Expr6 { EApp "!" [$2] }
      | '-' Expr6 { EApp "-" [$2] }
--       | '(' Type ')' Expr5 { ECast $2 $4 }
      | Expr6     { $1 }
Expr6 :: { Expr }
Expr6 : Identifier '(' listSep(Expr, ',') ')' { EApp $1 $3 }
      | Identifier                            { EVar $1 }
      | ExprPrimary                           { $1 }

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


-- Others --

ArrayMarks :: { Int }
ArrayMarks :                    { 0 }
           | '[' ']' ArrayMarks { $3 + 1 }

-- TypeSimple :: { Type }
-- TypeSimple : Identifier { TNamed $1 }
-- TODO: Change "identifier" to something more sensible


Type :: { Type }
Type : Identifier ArrayMarks { timesArray $2 (TNamed $1) }


Identifier :: { Identifier }
Identifier : identifier { $1 }


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
data TopLevel = TLFn FnDef | TLClass ClassDef
type ParseMonad = StateT Integer (Either String)

parseError :: [Token] -> ParseMonad a
parseError (t@(Token _ (AlexPn _ ln col)):_) =
    throwError $ "Parse error at (" ++ show ln ++ ", " ++ show col ++ "), "
              ++ "unexpected token " ++ show t
parseError [] = throwError "Parse error - unexpected end of file"

parse :: [Token] -> Either String Program
parse s = evalStateT (parseInner s) (globalScopeID + 1)

timesArray :: Int -> Type -> Type
timesArray n t = iterate TArray t !! n

makeBlock :: [Stmt] -> ParseMonad Stmt
makeBlock stms = state $ \i -> (Block i stms, i + 1)

desugarFor :: Type -> Identifier -> Expr -> Stmt -> ParseMonad Stmt
desugarFor t i e s = do
    inner <- makeBlock $
                  [ Decl t [Item i $ Just $ EApp "[]" [EVar "@for_array", EVar "@for_iterator"]]
                  , s
                  , Incr (EVar "@for_iterator")
                  ]
    makeBlock [ Decl (TNamed "int") [Item "@for_iterator" Nothing]
              , Decl (TArray t) [Item "@for_array" $ Just e]
              , Decl (TNamed "int") [Item "@for_length" $ Just $ EApp ".length" [EVar "@for_array"]]
              , While (EApp "<" [EVar "@for_iterator", EVar "@for_length"]) inner
              ]

programFromTLs :: [TopLevel] -> Program
programFromTLs [] = Program [] []
programFromTLs (TLFn fn : ls) =
    let Program fns cls = programFromTLs ls
    in Program (fn : fns) cls
programFromTLs (TLClass cs : ls) =
    let Program fns cls = programFromTLs ls
    in Program fns (cs : cls)
}

