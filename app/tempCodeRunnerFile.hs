run :: IO ()
run = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test.smr")
    case my_parse source of
        Left err   -> print err
        Right e    -> case exec e of
            Left err -> print err
            Right io -> io