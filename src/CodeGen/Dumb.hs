{-# LANGUAGE FlexibleContexts #-}
module CodeGen.Dumb(astToAsm) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe

import AST


data Context = Context { contextFunctionArguments :: [MangledIdentifier]
                       , contextStackVariables :: [MangledIdentifier]
                       }
type Mo = StateT Integer (WriterT String (ReaderT Context (Except String)))


astToAsm :: ProgramTyped -> Either String String
astToAsm (Program fns) = runExcept . flip runReaderT (Context [] [])
        . execWriterT $ flip evalStateT 0 $ do
            boilerplate
            mapM_ generateFunction fns


boilerplate :: Mo ()
boilerplate = do
    -- tellLn "global _start"
    tellLn "global main"
    tellLn "extern printInt"
    tellLn "extern __alloc_array"
    tellLn "extern error"
    tellLn "section .text"

    -- tellLn "_start:"
    -- tellLn "call main"
    -- tellLn "mov rdi, rax"
    -- tellLn "mov rax, 60"
    -- tellLn "syscall"


generateFunction :: FnDefTyped -> Mo ()
generateFunction (FnDef tRet ident args stmt) = do
    tellLn $ identifierLabel ident ++ ":"
    tellLn   "push rbp"
    tellLn   "mov rbp, rsp"
    local (const $ Context (map (\(Arg _ ident) -> ident) args) []) $ do
        generateStatement stmt
        -- TODO: Only generate if function type is void
        generateStatement VReturn


generateStatement :: StmtTyped -> Mo ()
generateStatement (Block bid stms) = do
    let locals = getLocalNames stms
    unless (null locals) $
        tellLn $ "sub rsp, " ++ show (length locals * 8)
    local (\(Context a b) -> Context a (b ++ locals)) $
        mapM_ generateStatement stms
    unless (null locals) $
        tellLn $ "add rsp, " ++ show (length locals * 8)

generateStatement (Assign e1 e2) = do
    generateLValue e1
    tellLn   "push rax"
    generateExpression e2
    tellLn   "pop rcx"
    tellLn   "mov [rcx], eax"

generateStatement (Decl t items) = do
    let work (Item ident Nothing) = do
            loc <- varBaseLoc ident
            tellLn $ "mov " ++ loc ++ ", 0"
        work (Item ident (Just e)) = do
            generateExpression e
            loc <- varBaseLoc ident
            tellLn $ "mov " ++ loc ++ ", rax"
    mapM_ work items

generateStatement Empty = tellLn "nop"

generateStatement (If e s1 s2) = do
    l1 <- nextTmpLabel
    l2 <- nextTmpLabel
    l3 <- nextTmpLabel
    generateExpression e
    tellLn   "test rax, rax"
    tellLn $ "jz " ++ l2
    tellLn $ l1 ++ ":"
    generateStatement s1
    tellLn $ "jmp " ++ l3
    tellLn $ l2 ++ ":"
    generateStatement s2
    tellLn $ l3 ++ ":"

generateStatement (While e s) = do
    lCond <- nextTmpLabel
    lBody <- nextTmpLabel
    lEnd <- nextTmpLabel
    tellLn $ lCond ++ ":"
    generateExpression e
    tellLn   "test rax, rax"
    tellLn $ "jz " ++ lEnd
    tellLn $ lBody ++ ":"
    generateStatement s
    tellLn $ "jmp " ++ lCond
    tellLn $ lEnd ++ ":"

generateStatement (SExpr e) =
    generateExpression e

generateStatement (Return e) = do
    generateExpression e
    generateStatement VReturn

generateStatement VReturn = do
    tellLn   "leave"
    tellLn   "ret"


getLocalNames :: [StmtTyped] -> [MangledIdentifier]
getLocalNames = concatMap worker
    where
        worker :: StmtTyped -> [MangledIdentifier]
        worker (Decl _ decls) = map (\(Item ident _) -> ident) decls
        worker _              = []


-- All expressions return their values in rax.
generateExpression :: ExprTyped -> Mo ()
generateExpression (EApp ident args) = functionCall ident args
generateExpression (EVar ident) = do
    loc <- varBaseLoc ident
    tellLn $ "mov rax, " ++ loc
generateExpression (EString s) = throwError "Strings are not supported yet!"
generateExpression (EBoolLiteral True) = tellLn "mov rax, 1"
generateExpression (EBoolLiteral False) = tellLn "xor rax, rax"
generateExpression (EIntLiteral i) = tellLn $ "mov rax, " ++ show i
generateExpression (ENew _ es) = functionCallDirect "__alloc_array" es


generateLValue :: ExprTyped -> Mo ()
generateLValue (EVar ident) = do
    loc <- varBaseLoc ident
    tellLn $ "lea rax, " ++ loc

generateLValue (EApp ident [e1, e2])
    | identifierLabel ident == "[]" = do
        calcTwoArguments e1 e2
        checkArrayBounds
        tellLn   "lea rax, [rcx + 8 * rax + 8]"
generateLValue _ = throwError "Expression is not a lvalue"


functionCall :: MangledIdentifier -> [ExprTyped] -> Mo ()
functionCall ident [e]
    | identifierLabel ident == "-" = do
        generateExpression e
        tellLn   "neg rax"
    | identifierLabel ident == "!" = do
        generateExpression e
        tellLn   "xor rax, 1"
    | identifierLabel ident == ".length" = do
        generateExpression e
        tellLn   "mov rax, [rax]"
functionCall ident [e1, e2]
    | identifierLabel ident `elem` ["+", "-"] = do
        calcTwoArguments e1 e2
        fromJust $ lookup (identifierLabel ident)
            [ ("+", tellLn "add rax, rcx")
            , ("-", tellLn "sub rcx, rax" >> tellLn "mov rax, rcx")]
    | identifierLabel ident == "*" = do
        calcTwoArguments e1 e2
        tellLn   "mov r10, rdx"
        tellLn   "xor rdx, rdx"
        tellLn   "mul rcx"
        tellLn   "mov rdx, r10"
    | identifierLabel ident `elem` ["/", "%"] = do
        calcTwoArguments e1 e2
        tellLn   "xchg rax, rcx"
        tellLn   "mov r10, rdx"
        tellLn   "mul rcx"
        when (identifierLabel ident == "%") $
            tellLn "mov rax, rdx"
        tellLn   "mov rdx, r10"
    | identifierLabel ident `elem` ["==", "!=", "<", "<=", ">", ">="] = do
        calcTwoArguments e1 e2
        tellLn   "cmp rcx, rax"
        let flag = fromJust $ lookup (identifierLabel ident)
                [ ("==", "e"), ("!=", "ne")
                , ("<", "l"), ("<=", "le")
                , (">", "g"), (">=", "ge")]
        tellLn $ "set" ++ flag ++ " al"
        tellLn   "and rax, 0xFF"
    | identifierLabel ident == "&&" = do
        l <- nextTmpLabel
        generateExpression e1
        tellLn   "test rax, rax"
        tellLn $ "jz " ++ l
        generateExpression e2
        tellLn $ l ++ ":"
    | identifierLabel ident == "||" = do
        l <- nextTmpLabel
        generateExpression e1
        tellLn   "test rax, rax"
        tellLn $ "jnz " ++ l
        generateExpression e2
        tellLn $ l ++ ":"
    | identifierLabel ident == "[]" = do
        calcTwoArguments e1 e2
        checkArrayBounds
        tellLn   "mov rax, [rcx + 8 * rax + 8]"
functionCall ident args = functionCallDirect (identifierLabel ident) args


functionCallDirect :: String -> [ExprTyped] -> Mo ()
functionCallDirect ident args = do
    -- Need to preserve registers containing arguments of current function
    argNum <- asks (length . contextFunctionArguments)
    forM_ (take argNum registersForArguments) $ \reg ->
        tellLn $ "push " ++ reg

    -- Push arguments on stack or move to registers
    let methods = map (\r -> "mov " ++ r ++ ", rax") registersForArguments
                    ++ repeat "push rax"
    forM_ (zip methods args) $ \(method, e) ->
        generateExpression e >> tellLn method

    tellLn $ "call " ++ ident

    -- Restore registers
    forM_ (reverse $ take argNum registersForArguments) $ \reg ->
        tellLn $ "pop " ++ reg


calcTwoArguments :: ExprTyped -> ExprTyped -> Mo ()
calcTwoArguments e1 e2 = do
    generateExpression e1
    tellLn   "push rax"
    generateExpression e2
    tellLn   "pop rcx"

checkArrayBounds :: Mo ()
checkArrayBounds = do
    tellLn   "cmp [rcx], rax"
    tellLn   "jbe error"


nextTmpLabel :: Mo String
nextTmpLabel = state $ \i -> (".L" ++ show i, i + 1)


varBaseLoc :: MangledIdentifier -> Mo String
varBaseLoc ident = do
    mArgOffset <- asks (elemIndex ident . contextFunctionArguments)
    case mArgOffset of
        Just off -> return $ if off < 6
                                then registersForArguments !! off
                                else "[rbp + " ++ show (off * 8 + 16) ++ "]"
        Nothing -> do
            Just offset <- asks (elemIndex ident . contextStackVariables)
            return $ "[rbp - " ++ show (offset * 8 + 8) ++ "]"


registersForArguments :: [String]
registersForArguments = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]


tellLn :: (MonadWriter String m) => String -> m ()
tellLn s = tell $ s ++ "\n"

