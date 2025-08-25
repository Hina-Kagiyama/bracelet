module Main where {
  import Format (formatModule);
  import Language.Haskell.Exts (ParseResult(ParseFailed, ParseOk), parseModule);
  main :: IO ();
  main = do {
    src <- getContents;
    case parseModule src of {
      ParseOk ast -> formatModule ast;
      ParseFailed loc msg -> putStrLn ("Parse error at " ++ show loc ++ ": " ++ msg)
    }
  }
}
