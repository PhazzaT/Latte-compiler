-- Hand-written grammar for Latte language
{
module Parser(parse) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import AST
import CompileError hiding (parseError)
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
    identifier      { Token (TokIdentifier _) _ }
    intLiteral      { Token (TokIntLiteral _) _ }
    stringLiteral   { Token (TokStrLiteral _) _ }
    boolLiteral     { Token (TokBoolLiteral _) _ }

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

Program :: { WLI Program }
Program : list(TopLevel) { programFromTLs `fmap` sequenceA $1 }

TopLevel :: { WLI TopLevel }
TopLevel : ClassDef { TLClass `fmap` $1 }
         | FnDef    { TLFn `fmap` $1 }

ClassDef :: { WLI ClassDef }
ClassDef : class Identifier '{' list(MemberDef) '}' { appL $ liftA4 (\_ x y _ -> ClassDef x $ concat y) (mktk $1) $2 (sequenceA $4) (mktk $5) }

MemberDef :: { WLI [(String, Type)] }
MemberDef : Type listSepNEmpty(Identifier, ',') ';' { liftA3 (\x y _ -> map (\z -> (z, x)) y) $1 (sequenceA $2) (mktk $3) }

FnDef :: { WLI FnDef }
FnDef : Type Identifier '(' listSep(Arg, ',') ')' Block { appL $ liftA4 FnDef $1 $2 (sequenceA $4) $6 }

Arg :: { WLI Arg }
Arg : Type Identifier { appL $ liftA2 Arg $1 $2 }


-- Statements --

Block :: { WLI Stmt }
Block : '{' list(Stmt) '}' {% fmap embedS $ sequenceA $ liftA3 (\_ x _ -> makeBlock x) (mktk $1) (sequenceA $2) (mktk $3) }

Stmt :: { WLI Stmt }
Stmt : Type listSepNEmpty(Item, ',') ';' { liftA3 (\x y _ -> Decl x y) $1 (sequenceA $2) (mktk $3) }
     | Stmt1                             { $1 }

Stmt1 :: { WLI Stmt }
Stmt1 : ';'                              { liftA (const Empty) (mktk $1) }
      | Expr '=' Expr ';'                { embedS $ liftA3 (\x y _ -> Assign x y) $1 $3 (mktk $4) }
      | Expr '++' ';'                    { embedS $ liftA2 (\x _ -> Incr x) $1 (mktk $3) }
      | Expr '--' ';'                    { embedS $ liftA2 (\x _ -> Decr x) $1 (mktk $3) }
      | return Expr ';'                  { embedS $ liftA3 (\_ x _ -> Return x) (mktk $1) $2 (mktk $3) }
      | return ';'                       { embedS $ liftA2 (\_ _ -> VReturn) (mktk $1) (mktk $2) }
      | if '(' Expr ')' Stmt1 else Stmt1 { embedS $ liftA4 (const If) (mktk $1) $3 $5 $7 }
      | if '(' Expr ')' Stmt1            { embedS $ liftA3 (\_ x y -> If x y Empty) (mktk $1) $3 $5 }
      | while '(' Expr ')' Stmt1         { embedS $ liftA3 (const While) (mktk $1) $3 $5 }
      | for '(' Type Identifier ':' Expr ')' Stmt1
                                         {% fmap embedS $ sequenceA $ liftA5 (const desugarFor) (mktk $1) $3 $4 $6 $8 }
      | Expr ';'                         { embedS $ liftA2 (\x _ -> SExpr x) $1 (mktk $2) }
      | Block                            { $1 }

Item :: { WLI Item }
Item : Identifier          { appL $ (\x -> Item x Nothing) `fmap` $1 }
     | Identifier '=' Expr { appL $ liftA2 (\x y -> Item x (Just y)) $1 $3 }


-- Expressions --

-- Based on official Java syntax
ExprPrimary :: { WLI Expr }
ExprPrimary : ExprPrimaryNoNewArray { $1 }
            | ExprArrayCreation     { $1 }
            | ExprObjectCreation    { $1 }

ExprPrimaryNoNewArray :: { WLI Expr }
ExprPrimaryNoNewArray : ExprLiteral     { $1 }
                      | ExprArrayAccess { $1 }
                      | ExprFieldAccess { $1 }
                      | '(' Expr ')'    { liftA3 (\_ x _ -> x) (mktk $1) $2 (mktk $3) }

ExprLiteral :: { WLI Expr }
ExprLiteral : stringLiteral { embedE $ EString `fmap` fromSLiteral $1 }
            | boolLiteral   { embedE $ EBoolLiteral `fmap` fromBLiteral $1 }
            | intLiteral    { embedE $ EIntLiteral `fmap` fromILiteral $1 }
            | null          { embedE $ const ENull `fmap` (mktk $1) }

ExprArrayAccess :: { WLI Expr }
ExprArrayAccess : Identifier '[' Expr ']'            { embedE $ liftA3 (\x y _ -> EApp "[]" [EVar x, y]) $1 $3 (mktk $4) }
                | ExprPrimaryNoNewArray '[' Expr ']' { embedE $ liftA3 (\x y _ -> EApp "[]" [x, y]) $1 $3 (mktk $4) }

ExprFieldAccess :: { WLI Expr }
ExprFieldAccess : Expr6 '.' Identifier { embedE $ liftA2 (\x y -> EApp ('.' : y) [x]) $1 $3 }

ExprArrayCreation :: { WLI Expr }
ExprArrayCreation : new Identifier '[' Expr ']' ArrayMarks
                        { embedE $ liftA3 (\x y z -> ENew (timesArray (z + 1) (TNamed x)) [y]) $2 $4 $6 }

ExprObjectCreation :: { WLI Expr }
ExprObjectCreation : new Identifier { embedE $ liftA2 (\_ x -> ENew (TNamed x) []) (mktk $1) $2 }

Expr :: { WLI Expr }
Expr : Expr1 '||' Expr { embedE $ liftA2 (\x y -> EApp "||" [x, y]) $1 $3 }
     | Expr1           { $1 }
Expr1 :: { WLI Expr }
Expr1 : Expr2 '&&' Expr1 { embedE $ liftA2 (\x y -> EApp "&&" [x, y]) $1 $3 }
      | Expr2            { $1 }
Expr2 :: { WLI Expr }
Expr2 : Expr2 RelOp Expr3 { embedE $ liftA2 (\x y -> EApp (show $2) [x, y]) $1 $3 }
      | Expr3             { $1 }
Expr3 :: { WLI Expr }
Expr3 : Expr3 AddOp Expr4 { embedE $ liftA2 (\x y -> EApp (show $2) [x, y]) $1 $3 }
      | Expr4             { $1 }
Expr4 :: { WLI Expr }
Expr4 : Expr4 MulOp Expr5 { embedE $ liftA2 (\x y -> EApp (show $2) [x, y]) $1 $3 }
      | Expr5             { $1 }
Expr5 :: { WLI Expr }
Expr5 : '!' Expr6 { embedE $ liftA2 (\_ x -> EApp "!" [x]) (mktk $1) $2 }
      | '-' Expr6 { embedE $ liftA2 (\_ x -> EApp "-" [x]) (mktk $1) $2 }
--       | '(' Type ')' Expr5 { ECast $2 $4 }
      | Expr6     { $1 }
Expr6 :: { wLI Expr }
Expr6 : Identifier '(' listSep(Expr, ',') ')' { embedE $ liftA3 (\x y _ -> EApp x y) $1 (sequenceA $3) (mktk $4) }
      | Identifier                            { embedE $ EVar `fmap` $1 }
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

ArrayMarks :: { WLI Int }
ArrayMarks :                    { pure 0 }
           | '[' ']' ArrayMarks { liftA2 (\_ x -> x + 1) (mktk $1) $3 }

-- TypeSimple :: { WLI Type }
-- TypeSimple : Identifier { TNamed $1 }
-- TODO: Change "identifier" to something more sensible


Type :: { WLI Type }
Type : Identifier ArrayMarks { liftA2 (\x y -> timesArray y (TNamed x)) $1 $2 }


Identifier :: { WLI Identifier }
Identifier : identifier { fromIdentifier $1 }


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
type ParseMonad = StateT Integer (Either PhaseError)
type WLI a = (LocInfo, a)

parseError :: [Token] -> ParseMonad a
parseError (t@(Token _ loc):_) =
    throwError $ PhaseError loc $ "Unexpected token " ++ show t
parseError [] = throwError $ PhaseError NoLocInfo "Parse error - unexpected end of file"

parse :: [Token] -> Either PhaseError Program
parse s = snd <$> evalStateT (parseInner s) (globalScopeID + 1)

timesArray :: Int -> Type -> Type
timesArray n t = iterate TArray t !! n

makeBlock :: [Stmt] -> ParseMonad Stmt
makeBlock stms = state $ \i -> (Block i stms, i + 1)

desugarFor :: Type -> Identifier -> Expr -> Stmt -> ParseMonad Stmt
desugarFor t i e s = do
    -- TODO: Add LocInfos
    inner <- makeBlock $
                  [ Decl t [Item i (Just $ EApp "[]" [EVar "@for_array", EVar "@for_iterator"]) NoLocInfo]
                  , s
                  , Incr (EVar "@for_iterator")
                  ]
    makeBlock [ Decl (TNamed "int") [Item "@for_iterator" Nothing NoLocInfo]
              , Decl (TArray t) [Item "@for_array" (Just e) NoLocInfo]
              , Decl (TNamed "int") [Item "@for_length" (Just $ EApp ".length" [EVar "@for_array"]) NoLocInfo]
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

fromIdentifier :: Token -> WLI String
fromIdentifier (Token (TokIdentifier s) loc) = (loc, s)

fromILiteral :: Token -> WLI Integer
fromILiteral (Token (TokIntLiteral i) loc) = (loc, i)

fromSLiteral :: Token -> WLI String
fromSLiteral (Token (TokStrLiteral s) loc) = (loc, s)

fromBLiteral :: Token -> WLI Bool
fromBLiteral (Token (TokBoolLiteral b) loc) = (loc, b)

liftA4 :: (Applicative f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

liftA5 :: (Applicative f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

mktk :: Token -> WLI TokenData
mktk (Token td loc) = (loc, td)

embedE :: WLI Expr -> WLI Expr
embedE (loc, e) = (loc, ELocInfo e loc)

embedS :: WLI Stmt -> WLI Stmt
embedS (loc, s) = (loc, SLocInfo s loc)

appL :: WLI (LocInfo -> a) -> WLI a
appL (loc, f) = (loc, f loc)
}

