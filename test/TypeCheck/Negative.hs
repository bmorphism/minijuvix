module TypeCheck.Negative (allTests) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Syntax.MicroJuvix.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: TypeCheckerError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint _file
            result <- runIOEither (upToMicroJuvixTyped entryPoint)
            case mapLeft fromMiniJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "The type checker did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not in the type checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "TypeCheck negative tests"
    (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Constructor in pattern type error"
      "MicroJuvix"
      "PatternConstructor.mjuvix"
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Type vs inferred type mismatch"
      "MicroJuvix"
      "WrongType.mjuvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      "MicroJuvix"
      "ExpectedFunctionType.mjuvix"
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unsolved hole"
      "MicroJuvix"
      "UnsolvedMeta.mjuvix"
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      "MicroJuvix"
      "MultiWrongType.mjuvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong return type name for a constructor of a simple data type"
      "MicroJuvix"
      "WrongReturnType.mjuvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too few arguments for the return type of a constructor"
      "MicroJuvix"
      "WrongReturnTypeTooFewArguments.mjuvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too many arguments for the return type of a constructor"
      "MicroJuvix"
      "WrongReturnTypeTooManyArguments.mjuvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest "E1" "MicroJuvix/NoStrictlyPositiveDataTypes" "E1.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E2" "MicroJuvix/NoStrictlyPositiveDataTypes" "E2.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E3" "MicroJuvix/NoStrictlyPositiveDataTypes" "E3.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E4" "MicroJuvix/NoStrictlyPositiveDataTypes" "E4.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E5" "MicroJuvix/NoStrictlyPositiveDataTypes" "E5.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E6" "MicroJuvix/NoStrictlyPositiveDataTypes" "E6.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E7" "MicroJuvix/NoStrictlyPositiveDataTypes" "E7.mjuvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError
  ]
