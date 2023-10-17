import HUnitSpec
import Test.HUnit

-- cf. https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit-Text.html#v:runTestTTAndExit
main :: IO ()
main = runTestTTAndExit tests