import AppendSpec


main :: IO ()
main = do
  boldItalicHappy <- appendBoldItalicSpecHappy
  case boldItalicHappy of 
    True -> putStrLn "\nOK"
    False -> putStrLn "\nFailed"

  boldItalicSad <- appendBoldItalicSpecSad
  case boldItalicSad of 
    True -> putStrLn "\nFailed"
    False -> putStrLn "\nOK"

