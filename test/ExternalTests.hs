module ExternalTests(runFileTests) where

import Control.Exception
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.Set as S

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Process

import Test.Hspec
import Test.HUnit hiding (TestCase)

import CompilePasses
import CompileError


data TestCase = TestCase { testCaseSource :: FilePath
                         , testCaseInput :: Maybe FilePath
                         , testCaseOutput :: Maybe FilePath }

data TestResult
    = Success
    | CompilationFail CompileErrorType
    | RuntimeFail
    | OutputMismatch
    deriving (Eq)


runFileTests :: Spec
runFileTests = do
    let run desc dir desired = describe desc $ join $ runIO $ runTestsInDirectory desired $ baseDir </> dir
        baseDir = "/home/phazzat/Pobrane/lattests/"

    run "Good tests" "good/" [Success]
    run "Bad tests" "bad/" [CompilationFail ParseError, CompilationFail TypeCheckError]


runTestsInDirectory :: [TestResult] -> FilePath -> IO Spec
runTestsInDirectory desired dir = do
    files <- sort <$> getDirectoryContents dir
    let names = filter (".lat" `isSuffixOf`) files
    let sFiles = S.fromList files
    let cases = flip map names $ \name ->
            let extChanged ext =
                    let replaced = replaceExtension name ext
                    in if replaced `S.member` sFiles
                          then Just $ dir </> replaced
                          else Nothing
                       in TestCase { testCaseSource = dir </> name
                                   , testCaseInput = extChanged "input"
                                   , testCaseOutput = extChanged "output" }
    return $ mapM_ (runExample desired) cases


runExample :: [TestResult] -> TestCase -> Spec
runExample desired tc = specify ("Test case: " ++ testCaseSource tc) $ do
    (result, msg) <- compilePhase
    unless (result `elem` desired) $
        expectationFailure $ "Expected " ++ enumerateList (map message desired)
                          ++ ", got " ++ message result ++ msg
    where
        compilePhase :: IO (TestResult, String)
        compilePhase = do
            source <- readFile (testCaseSource tc) >>= \s -> length s `seq` return s
            case compile source of
                Left (CompileError cet msg) -> return (CompilationFail cet, " - " ++ msg)
                Right result                -> assembleAndRun result

        assembleAndRun :: String -> IO (TestResult, String)
        assembleAndRun asm =
            bracket
                (openBinaryTempFile "/tmp/" "espresso-output")
                (\(path, h) -> hClose h >> removeFile path)
                $ \(path, h) -> do
                    (Just hin, _, _, ph)
                        <- createProcess (proc "gcc" ["-o", "/dev/stdout", stdLib, "-x", "assembler", "-"])
                                                                            { std_in = CreatePipe
                                                                            , std_out = UseHandle h
                                                                            , std_err = Inherit }
                    hPutStr hin asm
                    hClose hin
                    code <- waitForProcess ph
                    case code of
                        ExitFailure i -> ioError $ userError $ "'gcc' returned with code " ++ show i
                        ExitSuccess   -> return ()

                    bracket
                        (traverse (`openFile` ReadMode) $ testCaseInput tc)
                        (`whenJust` hClose)
                        $ \mhin -> do
                            setFileMode path $ unionFileModes ownerReadMode ownerExecuteMode
                            (mhin2, mhout, _, ph2)
                                <- createProcess (proc path []) { std_in = maybe Inherit UseHandle mhin
                                                                , std_out = if isNothing $ testCaseOutput tc
                                                                               then Inherit -- *grumble*
                                                                               else CreatePipe }

                            -- TODO: Check exit code
                            case testCaseOutput tc of
                                Just p -> withFile p ReadMode $ \hout2 -> do
                                    let Just hout1 = mhout
                                    s1 <- hGetContents hout1 >>= \s -> length s `seq` return s
                                    s2 <- hGetContents hout2 >>= \s -> length s `seq` return s
                                    code <- waitForProcess ph2
                                    checkExitCode code $
                                        if s1 == s2
                                           then return (Success, "")
                                           else return (OutputMismatch, " - " ++ show s1 ++ " vs " ++ show s2)
                                Nothing -> do
                                    code <- waitForProcess ph2
                                    checkExitCode code $
                                        return (Success, "")

        checkExitCode :: ExitCode -> IO (TestResult, String) -> IO (TestResult, String)
        checkExitCode (ExitFailure i) _ = return (RuntimeFail, " - exit code: " ++ show i)
        checkExitCode ExitSuccess     m = m

        message :: TestResult -> String
        message Success             = "success"
        message (CompilationFail t) = "compilation failure with " ++ show t
        message RuntimeFail         = "runtime error"
        message OutputMismatch      = "output mismatch"


-- Screw haskell stack and cabal with their treatment of data files during tests
stdLib :: FilePath
stdLib = "/home/phazzat/mim/mrjp/Espresso/stdlib/lib.o"


whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = pure ()
whenJust (Just x) f = f x


-- TODO: Duplicate from SpecialFunctions
enumerateList :: (Show a) => [a] -> String
enumerateList [x] = show x
enumerateList [x, y] = show x ++ " or " ++ show y
enumerateList (l:ls) = show l ++ ", " ++ enumerateList ls
enumerateList [] = ""

