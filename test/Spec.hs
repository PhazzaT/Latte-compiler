{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.Generics
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Monoid

import AST
import CompilePasses(lexAndParse, typeCheck)


main :: IO ()
main = hspec $ do
    let makeFun s = "void foo() { " ++ s ++ " }" :: String
    describe "Parsing" $ do
        let parsesOK  s = lexAndParse s `shouldSatisfy` isRight
        let parsesBad s = lexAndParse s `shouldNotSatisfy` isRight

        -- Correct programs
        it "parses empty source" $
            parsesOK ""

        it "parses empty function" $
            parsesOK $ makeFun ""

        it "parses semicolon" $
            parsesOK $ makeFun ";"

        it "parses naked integer literals" $ property $
            \i -> isRight . lexAndParse . makeFun $ show (i :: Int64) ++ ";"

        it "parses naked string literals" $
            parsesOK $ makeFun "\"The quick brown fox...\";"

        it "parses escape codes in string literals" $
            parsesOK $ makeFun "\"\\n\\t\\\\\";"

        it "parses naked boolean expressions" $
            parsesOK $ makeFun "true; false;"

        it "parses lower and upper case letters in symbol names" $
            parsesOK "void JustAFunction() {}"

        it "handles numbers in symbol names" $
            parsesOK "void t1337() {}"

        it "handles special symbols in symbol names" $
            parsesOK "void just_a_function() {}"

        it "parses addition"       $ parsesOK $ makeFun "40 + 2;"
        it "parses negation"       $ parsesOK $ makeFun "-2;"
        it "parses subtraction"    $ parsesOK $ makeFun "44 - 2;"
        it "parses multiplication" $ parsesOK $ makeFun "6 * 7;"
        it "parses division"       $ parsesOK $ makeFun "84 / 2;"
        it "parses comparisons" $
            parsesOK $ makeFun "1 < 2; 2 > 1; 1 >= 1; 1 <= 1; 1 == 1; 1 != 2;"

        it "parses boolean operators" $
            parsesOK $ makeFun "true || false; false && true; !true;"

        it "parses function application" $
            parsesOK $ makeFun "bar(1, true, \"The quick brown fox...\");"

        it "parses parameters in function declarations" $
            parsesOK "void foo(int x, string y) {}"

        it "parses local variable declarations" $
            parsesOK $ makeFun "int x; int y = 42;"

        it "parses if statement" $
            parsesOK $ makeFun "if (true) {}"

        it "parses if-else statement" $
            parsesOK $ makeFun "if (true) {} else {}"

        it "parses while statement" $
            parsesOK $ makeFun "while (true) {}"

        it "parses return statement" $
            parsesOK $ makeFun "return;"

        it "parses return with value statement" $
            parsesOK $ makeFun "return 42;"

        it "parses multi-line comments" $
            parsesOK $ makeFun $ "/* Eeny, meeny, miny, moe,\n"
                              ++ "Catch a tiger by the toe,\n"
                              ++ "If he hollers, let him go,\n"
                              ++ "Eeny, meeny, miny, moe. */"

        it "parses single-line hash comments" $
            parsesOK $ "# This is just a comment.\n"
                    ++ "void foo() {}\n"
                    ++ "# This is also a comment.\n"
                    ++ "void bar() {} # And this is a comment, too!"

        it "parses single-line double slash comments" $
            parsesOK $ "// This is just a comment.\n"
                    ++ "void foo() {}\n"
                    ++ "// This is also a comment.\n"
                    ++ "void bar() {} // And this is a comment, too!"

        it "parses expressions with arrays" $
            parsesOK $ makeFun "array[123];"

        it "parses array creation expressions" $
            parsesOK $ makeFun "new int[1][];"

        it "parses multi-dimensional array expressions" $
            parsesOK $ makeFun "array[1][2][3];"

        it "parses simple array types" $
            parsesOK $ makeFun "int[] array;"

        it "parses multi-dimensional array types" $
            parsesOK $ makeFun "int[][][] chunk;"

        it "parses array length expression" $
            parsesOK $ makeFun "int[] arr; int l = arr.length;"

        it "parses array assignment" $
            parsesOK $ makeFun "arr[42] = 16;"

        -- Incorrect programs
        it "refuses unmatched function curly braces" $
            parsesBad "void foo() {"

        it "refuses wrong escape codes" $
            parsesBad $ makeFun "\"\\Q\";"

        it "refuses unmatched quotes, ignoring escaped ones" $
            parsesBad $ makeFun "\"Text\\\";"


    describe "Type checking" $ do
        let tFun = TFunction
        let tc = lexAndParse >=> typeCheck
        let tcOK s = tc s `shouldSatisfy` isRight
        let tcBad s = tc s `shouldNotSatisfy` isRight
        let hasIdentifiers ids p = all (`elem` allIdents) ids
                where
                    allIdents = everything (++) ([] `mkQ` select) p
                    select m = [m] :: [MangledIdentifier]
        let mid = MangledIdentifier

        it "typechecks an empty program" $
            tcOK ""

        it "typechecks void function" $
            tcOK "void foo() { return; }"

        it "typechecks non-void function" $
            tcOK "int foo() { return 42; }"

        it "refuses returning incorrect type from a function" $
            tcBad "int foo() { return \"The quick brown fox...\"; }"

        it "assigns correct types to declared variables" $
            tc "void foo() { int i; string s; }"
                `shouldSatisfy` hasIdentifiers [ mid "i" tInt [1, 0]
                                               , mid "s" tString [1, 0] ]

        it "assigns correct types to functions" $
            tc "void foo(string what, int times) { return; }"
                `shouldSatisfy` hasIdentifiers [ mid "foo" (tFun tVoid [tString, tInt]) [0] ]

        it "assigns correct types to arguments" $
            tc "int gcd(int a, int b) { return 42; }"
                `shouldSatisfy` hasIdentifiers [ mid "a" tInt [1, 0]
                                               , mid "b" tInt [1, 0] ]

        it "refuses two declarations of the same name in the same block" $
            tcBad $ makeFun "int x = 42; string x = \"The quick brown fox...\";"

        it "refuses two function declarations of the same name" $
            tcBad "int foo() {} void foo() {}"
        
        it "allows for name shadowing in different scopes" $
            tc (makeFun "int x = 42; { string x = \"The quick brown fox...\"; }")
                `shouldSatisfy` hasIdentifiers [ mid "x" tString [1, 2, 0]
                                               , mid "x" tInt [2, 0] ]
                                         -- See src/Parser.y for explanation on
                                         -- the scope numbering convention

        it "typechecks arithmetic operators" $
            tcOK $ makeFun "int x = (-10 + 5) * (1520 % 6 / 9 - 5);"

        it "typechecks string concatenation" $
            tcOK $ makeFun "string s = \"The quick \" + \"brown fox...\";"

        it "refuses addition of wrong types" $
            tcBad $ makeFun "42 + \"The quick brown fox...\""

        it "typechecks boolean operators" $
            tcOK $ makeFun "bool x = true || false && true;"

        it "typechecks comparison operators" $
            tcOK $ makeFun $ "bool x = (1 < 2) && (1 <= 2) && (2 > 1) && (2 >= 1)"
                               ++ " && (1 == 1) && (1 != 2);"

        it "enforces bool type in if expression" $
            tcBad $ makeFun "if (123) {}"

        it "enforces bool type in while expression" $
            tcBad $ makeFun "while (123) {}"

        it "typechecks expressions with variables" $
            tcOK $ makeFun "int x = 0; int y = x + 1;"
        
        it "refuses expressions with variables with incompatible types" $
            tcBad $ makeFun "string x; int y = x + 1;"

        it "typechecks function calls" $
            tcOK "int helper(int y) { return 42; } void foo() { int x = helper(1337); }"

        it "refuses argument type mismatch in function calls" $
            tcBad "int helper(int y) { return 42; } void foo() { int x = helper(\"Something\"); }"

        it "refuses type mismatch caused by incorrect return type" $
            tcBad "int helper(int y) { return 42; } void foo() { string x = helper(1337); }"

        it "does not take into account function declaration order" $
            tcOK "int foo() { return bar(); } int bar() { return foo(); }"

        it "typechecks array expressions" $
            tcOK $ makeFun "int[] tab; int y = tab[0] + tab[1];"

        it "typechecks array new expressions" $
            tcOK $ makeFun "int x = (new int[1][][])[1][2][3];"

        it "enforces int type as array length" $
            tcOK $ makeFun "int[] tab = new int[5]; printInt(tab.length);"

        it "refuses indexing an array with a non-int" $
            tcBad $ makeFun "int[] tab; tab[\"two\"];"

        it "typechecks multi-dimensional arrays" $
            tcOK . makeFun $
                   "int[][][] chunk;"
                ++ "int[][] slice = chunk[0];"
                ++ "int[] row = slice[0];"
                ++ "int corner = chunk[0][0][0];"

        it "refuses unknown types" $
            tcBad "thingamajig foo(whatsamacallit x) {}"
