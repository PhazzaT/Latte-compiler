{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module SpecialFunctions(returnTypeOfSpecialFunction) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe

import AST


type LookupFun = forall m. (MonadError String m) => Identifier -> [Type] -> MaybeT m Type


returnTypeOfSpecialFunction :: (MonadError String m) => Identifier -> [Type] -> m (Maybe Type)
returnTypeOfSpecialFunction f args = runMaybeT . msum . map (\fn -> fn f args) $
        [checkSimpleOperators, checkEqualityOperators, checkArrayLookupOperator]


checkSimpleOperators :: LookupFun
checkSimpleOperators f args = do
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
                            Nothing -> throwError $
                                "Unknown overload of operator " ++ f
                                 ++ " with arguments " ++ enumerateList args
                            Just t  -> breakWith t


checkEqualityOperators :: LookupFun
checkEqualityOperators f [t1, t2]
    | f `elem` ["==", "!="] = if t1 == t2
                                 then breakWith tBool
                                 else throwError $ "Compared types (with " ++ f
                                                ++ ") are different: "
                                                ++ show t1 ++ " vs. " ++ show t2
    | otherwise = continue
checkEqualityOperators _ _ = continue


checkArrayLookupOperator :: LookupFun
checkArrayLookupOperator "[]" [tArr, tInd] = do
    unless (tInd == tInt) $ throwError "Only an int can be an array index"
    case tArr of
        TArray tInner -> breakWith tInner
        _             -> throwError $ show tArr ++ " is not an array type"
checkArrayLookupOperator _ _ = continue


enumerateList :: (Show a) => [a] -> String
enumerateList [] = ""
enumerateList [x] = show x
enumerateList [x, y] = show x ++ " and " ++ show y
enumerateList (l:ls) = show l ++ ", " ++ enumerateList ls


breakWith :: (Monad m) => a -> MaybeT m a
breakWith = MaybeT . return . Just


continue :: (Monad m) => MaybeT m a
continue = MaybeT $ return Nothing

