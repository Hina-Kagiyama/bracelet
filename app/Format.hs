module Format (formatModule) where {
  import Data.Char (isSpace);
  import Language.Haskell.Exts (Module);
  import Language.Haskell.Exts.Pretty;
  formatModule :: (Show a) => Module a -> IO ();
  formatModule = putStrLn . reformat . prettyPrintStyleMode oneLineStyle defaultMode{
    layout = PPSemiColon
  } where {
    oneLineStyle = style{
      mode = OneLineMode
    }
  };
  data State = Normal | InString | InChar | Escape State deriving (Eq);
  reformat :: String -> String;
  reformat = go 0 Normal where {
    go _ _ [] = [];
    go indent Normal ('{' : xs) = "{" ++ "\n" ++ replicate (indent + 2) ' ' ++ go (indent + 2) Normal (dropWhile isSpace xs);
    go indent Normal (';' : xs) = ";" ++ "\n" ++ replicate indent ' ' ++ go indent Normal (dropWhile isSpace xs);
    go indent Normal ('}' : xs) = "\n" ++ replicate (indent - 2) ' ' ++ "}" ++ go (indent - 2) Normal xs;
    go indent Normal ('"' : xs) = '"' : go indent InString xs;
    go indent Normal ('\'' : xs) = '\'' : go indent InChar xs;
    go indent InString ('"' : xs) = '"' : go indent Normal xs;
    go indent InChar ('\'' : xs) = '\'' : go indent Normal xs;
    go indent st ('\\' : xs) = '\\' : go indent (Escape st) xs;
    go indent (Escape st) (c : xs) = c : go indent st xs;
    go indent st (c : xs) = c : go indent st xs
  }
}
