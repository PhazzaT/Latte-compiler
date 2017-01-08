{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module SpecialFunctions(returnTypeOfSpecialFunction) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M

import AST
import CompileError
import Lexer
import Utility


type LookupFun = forall m. (MonadError PhaseError m, MonadReader LocInfo m) => ClassInfo -> Identifier -> [Type] -> MaybeT m Type


returnTypeOfSpecialFunction :: (MonadError PhaseError m, MonadReader LocInfo m) => ClassInfo -> Identifier -> [Type] -> m (Maybe Type)
returnTypeOfSpecialFunction ci f args = runMaybeT . msum . map (\fn -> fn ci f args) $
        [ checkSimpleOperators, checkEqualityOperators
        , checkArrayLookupOperator, checkGetFieldOperator
        ]


checkSimpleOperators :: LookupFun
checkSimpleOperators _ f args = do
    let utype tfrom tto = ([tfrom], tto)
    let btype tfrom tto = ([tfrom, tfrom], tto)
    let operators = [ ("+",  [btype tInt tInt, btype tString tString])
                    , ("-",  [btype tInt tInt, utype tInt tInt])
                    , ("*",  [btype tInt tInt])
                    , ("/",  [btype tInt tInt])
                    , ("%",  [btype tInt tInt])
                    , ("&&", [btype tBool tBool])
                    , ("||", [btype tBool tBool])
                    , ("<",  [btype tInt tBool])
                    , ("<=", [btype tInt tBool])
                    , (">",  [btype tInt tBool])
                    , (">=", [btype tInt tBool])
                    , ("!",  [utype tBool tBool])
                    ]

    case lookup f operators of
        Nothing        -> continue
        Just overloads -> case lookup args overloads of
                            Nothing -> throwErrorRLoc $
                                "Unknown overload of operator " ++ f
                                 ++ " with arguments " ++ enumerateList args
                            Just t  -> breakWith t


checkEqualityOperators :: LookupFun
checkEqualityOperators _ f [t1, t2]
    | f `elem` ["==", "!="] = if typesEqualityComparable t1 t2
                                 then breakWith tBool
                                 else throwErrorRLoc $ "Types are incomparable: "
                                                ++ show t1 ++ " vs. " ++ show t2
    | otherwise = continue
checkEqualityOperators _ _ _ = continue


typesEqualityComparable :: Type -> Type -> Bool
typesEqualityComparable TNull t = isNullable t
typesEqualityComparable t TNull = isNullable t
typesEqualityComparable t1 t2 = t1 == t2


isNullable :: Type -> Bool
isNullable TNull = True
isNullable (TArray _) = True
isNullable (TNamed _) = True
isNullable _ = False


checkArrayLookupOperator :: LookupFun
checkArrayLookupOperator _ "[]" [tArr, tInd] = do
    unless (tInd == tInt) $ throwErrorRLoc "Only an int can be an array index"
    case tArr of
        TArray tInner -> breakWith tInner
        _             -> throwErrorRLoc $ show tArr ++ " is not an array type"
checkArrayLookupOperator _ _ _ = continue


checkGetFieldOperator :: LookupFun
checkGetFieldOperator _ ".length" [TArray _] = breakWith tInt
checkGetFieldOperator ci ('.':mb) [TNamed n] =
    case M.lookup n ci >>= lookup mb of
        Just t -> breakWith t
        Nothing -> throwErrorRLoc $ "Class " ++ n ++ " has no field \"" ++ mb ++ "\""
checkGetFieldOperator _ _ _ = continue


enumerateList :: (Show a) => [a] -> String
enumerateList [] = ""
enumerateList [x] = show x
enumerateList [x, y] = show x ++ " and " ++ show y
enumerateList (l:ls) = show l ++ ", " ++ enumerateList ls


breakWith :: (Monad m) => a -> MaybeT m a
breakWith = MaybeT . return . Just


continue :: (Monad m) => MaybeT m a
continue = MaybeT $ return Nothing

