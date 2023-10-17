module HUnitSpec where

import Test.HUnit
import AppendSpec

assertIOBoolHappy :: String -> IO Bool -> Assertion
assertIOBoolHappy msg iob = assertBool msg =<< iob

assertIOBoolSad :: String -> IO Bool -> Assertion
assertIOBoolSad msg iob = assertBool msg =<< (not <$> iob)

test1 = TestCase $ assertIOBoolHappy "failure test1" appendBoldItalicSpecHappy
test2 = TestCase $ assertIOBoolSad "failure test2" appendBoldItalicSpecSad

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]