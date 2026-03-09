{-|
Module      : TestHS
Description : A lightweight testing framework for Haskell
Copyright   : Thodoris Papakonstantinou, 2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

A minimal pure-functional test harness.  Build a list of 'Test' values
using 'testPassed' and 'testFailed', then report results with
'reportTests' (pure tests) or 'reportTestsIO' (IO tests).
Exits with failure if any test fails.
 -}
module TestHS
    ( Test
    , runTest
    , reportTests
    , reportTestsIO
    , testPassed
    , testFailed
    ) where

import Data.Tuple
import Control.Monad
import System.Console.ANSI
import System.Exit


-- | Test data type
data Test = Test
  { name :: String
  , outcome :: Either (String, String) String
  } deriving (Show, Eq)

-- | Create a passing test with a name and a result message.
testPassed :: String -> String -> Test
testPassed t s = Test 
  { name = t
  , outcome = Right s
  }
  
-- | Create a failing test with a name and an @(expected, got)@ pair.
testFailed :: String -> (String,String) -> Test
testFailed t f = Test 
  { name = t
  , outcome = Left f
  }
 
-- | run pure test
runTest :: Test -> IO Test
runTest t = do
  case outcome t of
    Left err -> do
      putStr $ name t
      putStr $ " expected: " ++ fst err
      putStr $ " got: " ++ snd err
      setSGR [SetColor Foreground Vivid Red]
      putStrLn  " ✗" 
      setSGR [Reset]
    Right succe -> do
      putStr $ name t
      putStr $ ": " ++ succe 
      setSGR [SetColor Foreground Vivid Green]
      putStrLn " ✓"
      setSGR [Reset]
  return t

-- | Run a list of pure tests, print results, and exit with failure if any fail.
reportTests :: [Test] -> IO ()
reportTests ts = do
  tests <- sequence $ map runTest ts
  let lt = length tests
  let passedtests = filter 
                    (\test -> case outcome test of 
                      Left _ -> False
                      Right _ -> True)
                      tests
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  case passedAll of
       True -> do
         putStrLn $ "Passed all " ++ (show lt) ++ " tests!! 🎉"
       False -> do
         putStrLn $ "Failed "  ++ (show failedTests) ++ " test(s) 😣"
         exitFailure

-- | Run tests with IO
reportTestsIO :: [IO Test] -> IO ()
reportTestsIO ts = do
  putStrLn "Running tests"
  testsIO <- sequence ts
  putStrLn "Reporting tests"
  tests <- sequence $ map runTest testsIO
  let lt = length tests
  let passedtests = filter 
                    (\test -> case outcome test of 
                      Left _ -> False
                      Right _ -> True)
                      tests
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  case passedAll of
       True -> do
         putStrLn $ "Passed all " ++ (show lt) ++ " tests!! 🎉"
       False -> do
         putStrLn $ "Failed "  ++ (show failedTests) ++ " test(s) 😣"
         exitFailure
