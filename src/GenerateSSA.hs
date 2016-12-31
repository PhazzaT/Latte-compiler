module GenerateSSA where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List
import Data.Maybe

import qualified AST

import Utility


-- data Graph = Graph { vertices :: M.Map BlockID Block
--                    , edges    :: M.Map BlockID [BlockID]
--                    } deriving (Eq, Show)

type BlockID = Integer
data Block = Block [TACode] deriving (Eq)

instance Show Block where
    show (Block l) = intercalate "\n" $ map show l

-- Three address code
data TACode = Assign Arg Arg
            | Jump BlockID
            | CondJump Arg BlockID BlockID
            | Call AST.MangledIdentifier [Arg] Register
            | Return (Maybe Arg)
            | Label BlockID
            deriving (Eq)

instance Show TACode where
    show (Assign a1 a2)            = "\t" ++ show a1 ++ " := " ++ show a2
    show (Jump bid)                = "\t@jump #" ++ show bid
    show (CondJump a bid1 bid2)    = "\t@jumpif " ++ show a ++ ", " ++ show bid1 ++ ", " ++ show bid2
    show (Call ident args reg)     = "\t" ++ showRegister reg ++ " := @call "
                                        ++ AST.identifierLabel ident
                                        ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (Return ma)               = "\t@return" ++ maybe "" ((" "++) . show) ma
    show (Label bid)               = "#" ++ show bid ++ ":"

data Arg = Reg Register
         | BoolConstant Bool
         | IntConstant Integer
         deriving (Eq)

instance Show Arg where
    show (Reg r) = showRegister r
    show (BoolConstant False) = "$false"
    show (BoolConstant True) = "$true"
    show (IntConstant i) = "$" ++ show i

type Register = String

showRegister :: Register -> String
showRegister r = '%' : r

type TAMonad = StateT TAState (Except String)
data TAState = TAState { nextTempRegister   :: Integer
                       , nextLabel          :: Integer
                       , regNumbers         :: TARegNumbers
                       } deriving (Eq, Show)
type TARegNumbers = M.Map AST.MangledIdentifier Integer


astToSSA :: AST.ProgramTyped -> Either String (M.Map AST.MangledIdentifier [TACode])
astToSSA (AST.Program fns) =
    let fns' = fixVoidFunctions fns
    in fmap M.fromList $ runExcept $ (`evalStateT` TAState 0 0 M.empty) $
        forM fns' $ \(AST.FnDef tRet ident args stmt) -> do
            -- Bring function arguments into scope
            forM_ args $ \(AST.Arg _ ident) -> allocRegForVariable ident
            code <- convertStmt stmt
            return (ident, code)


fixVoidFunctions :: [AST.FnDefTyped] -> [AST.FnDefTyped]
fixVoidFunctions = map work
    where
        work f@(AST.FnDef tRet ident args (AST.Block bid stms))
            | tRet == AST.tVoid = AST.FnDef tRet ident args $ AST.Block bid $ stms ++ [AST.VReturn]
            | otherwise = f


convertStmt :: AST.StmtTyped -> TAMonad [TACode]
convertStmt (AST.Assign expr1 expr2) = do
    (eCode1, arg1) <- convertExpr expr1
    (eCode2, arg2) <- convertExpr expr2
    return $ eCode1 ++ eCode2 ++ [Assign arg1 arg2]
convertStmt (AST.Block _ stmts)     = concat <$> mapM convertStmt stmts
convertStmt (AST.Decl t items)      = concat <$> mapM (convertItem t) items
convertStmt AST.Empty               = return []
convertStmt (AST.If cond s1 s2)     = do
    (condCode, a) <- convertExpr cond
    sCode1 <- convertStmt s1
    sCode2 <- convertStmt s2
    l1 <- allocLabel
    l2 <- allocLabel
    lEnd <- allocLabel
    return $ condCode
          ++ [CondJump a l1 l2]
          ++ [Label l1]
          ++ sCode1
          ++ [Jump lEnd]
          ++ [Label l2]
          ++ sCode2
          ++ [Jump lEnd]
          ++ [Label lEnd]
convertStmt (AST.Return e)          = do
    (code, a) <- convertExpr e
    return $ code ++ [Return $ Just a]
convertStmt (AST.SExpr e)           = fst <$> convertExpr e
convertStmt AST.VReturn             = return [Return Nothing]
convertStmt (AST.While cond s)      = do
    (condCode, a) <- convertExpr cond
    sCode <- convertStmt s
    lCond <- allocLabel
    lLoop <- allocLabel
    lEnd <- allocLabel
    return $ [Label lCond]
          ++ condCode
          ++ [CondJump a lLoop lEnd]
          ++ [Label lLoop]
          ++ sCode
          ++ [Jump lCond]
          ++ [Label lEnd]


convertItem :: AST.Type -> AST.ItemTyped -> TAMonad [TACode]
convertItem t (AST.Item ident me) = do
    let defValue
            | t == AST.tBool   = BoolConstant False
            | t == AST.tString = error "Strings are not supported yet!"
            | t == AST.tInt    = IntConstant 0
            | otherwise        = error "User defined types are not supported yet!"
    (code, reg) <- fromMaybe (return ([], defValue)) $ convertExpr <$> me
    reg' <- Reg <$> allocRegForVariable ident
    return $ code ++ [Assign reg' reg]


convertExpr :: AST.ExprTyped -> TAMonad ([TACode], Arg)
convertExpr (AST.EString s)       = error "Strings are not supported yet!"
convertExpr (AST.EApp ident args) = do
    ret <- allocReg
    (codes, args') <- unzip <$> mapM convertExpr args
    return (concat codes ++ [Call ident args' ret], Reg ret)
convertExpr (AST.EBoolLiteral b)  = return ([], BoolConstant b)
convertExpr (AST.EIntLiteral i)   = return ([], IntConstant i)
convertExpr (AST.EVar v)          = do
    reg <- Reg <$> getRegForVariable v
    return ([], reg)


allocReg :: TAMonad Register
allocReg = do
    num <- gets nextTempRegister
    modify $ \s -> s { nextTempRegister = num + 1 }
    return $ "temp#" ++ show num


allocRegForVariable :: AST.MangledIdentifier -> TAMonad Register
allocRegForVariable ident = do
    rnums <- gets regNumbers
    let nextNum = maybe 0 (+1) $ M.lookup ident rnums
    modify $ \c -> c { regNumbers = M.insert ident nextNum $ regNumbers c }
    return $ getMangledName ident ++ "#" ++ show nextNum


getRegForVariable :: AST.MangledIdentifier -> TAMonad Register
getRegForVariable ident = do
    rnums <- gets regNumbers
    num <- case M.lookup ident rnums of
        Nothing -> throwError $ "Internal error - could not get register for variable " ++ show ident
        Just n  -> return n
    return $ getMangledName ident ++ "#" ++ show num
    

allocLabel :: TAMonad BlockID
allocLabel = do
    num <- gets nextLabel
    modify $ \s -> s { nextLabel = num + 1 }
    return num


getMangledName :: AST.MangledIdentifier -> String
getMangledName (AST.MangledIdentifier ident t bids) = ident ++ "@" ++ show t ++ "@" ++ show (head bids)


-- observeRegisterChanges :: TAMonad a -> TAMonad (a, TARegNumbers)
-- observeRegisterChanges
