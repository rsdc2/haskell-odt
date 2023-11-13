module HUnitSpec where

import Test.HUnit
import UtilsSpec
import AppendSpec
import QuerySpec


test1 = TestCase $ assertIOBoolHappy "failure test1" appendBoldItalicTextStyleWithSpanHappy
test2 = TestCase $ assertIOBoolSad "failure test2" appendBoldItalicTextStyleWithSpanSad
test3 = TestCase $ assertIOBoolHappy "failure test3" appendItalicParaStyleWithParaHappy
test4 = TestCase $ assertIOBoolSad "failure test4" appendItalicParaStyleWithParaSad
test5 = TestCase $ assertIOBoolHappy "failure test5" prependBoldItalicTextStyleWithSpanHappy
test6 = TestCase $ assertIOBoolHappy "failure test6" prependItalicParaStyleWithParaHappy
test7 = TestCase $ assertIOBoolHappy "failure test7" appendToODTEqAppendToDoc
test8 = TestCase $ assertIOBoolHappy "failure test8" prependToODTEqAppendToDoc
test9 = TestCase $ assertIOBoolHappy "Test appendParaToArchive failed" appendParaToArchive
test10 = TestCase $ assertIOBoolHappy "Test appendParaToArchive' failed" appendParaToArchive'

test20 = TestCase $ assertIOBoolHappy "Test lastParaHappy failed" lastParaHappy

tests = TestList [
      TestLabel "test1" test1
    , TestLabel "test2" test2
    , TestLabel "test3" test3
    , TestLabel "test4" test4
    , TestLabel "test5" test5
    , TestLabel "test6" test6
    , TestLabel "test7" test7
    , TestLabel "test8" test8
    , TestLabel "test9" test9
    , TestLabel "test10" test10
    , TestLabel "test20" test8]