import System.IO

import ArithParser

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     let (numer,denom) = eval . parse . tokenize $ line
     putStrLn ((show numer) ++ " / " ++ (show denom))
     main
