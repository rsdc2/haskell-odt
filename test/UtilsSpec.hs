module UtilsSpec where
    
import Test.HUnit

assertIOBoolHappy :: String -> IO Bool -> Assertion
assertIOBoolHappy msg iob = assertBool msg =<< iob

assertIOBoolSad :: String -> IO Bool -> Assertion
assertIOBoolSad msg iob = assertBool msg =<< (not <$> iob)