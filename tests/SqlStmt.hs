module Main
    ( main
    ) where

import Data.SQL.Parse
import System.Environment

pparse :: String -> IO ()
pparse str =
    case parse str of
        Right (s, [])      -> putStrLn ("SUCCESS: " ++ show s)
        Right (s, e@(_:_)) -> putStrLn ("REMAINING: " ++ show s ++ " => " ++ show e)
        Left err           -> putStrLn ("ERROR: " ++ show err)

mainX :: IO ()
mainX = do
    pparse "SELECT * FROM table"
    pparse "SELECT a,b FROM table WHERE a > 0"
    pparse "SELECT a,MEAN(b) FROM table WHERE a > 0"

    pparse "SELECT a,MEAN(b) FROM table WHERE a > 0 AND b > 0 OR b < 0 AND c > 0"

    pparse "SELECT d.given_on FROM   documents_document d WHERE  d.given_on >= '2001-01-01 0:0' AND    d.given_on <  '2001-02-01 0:0' ORDER  BY d.created_on DESC"

    pparse "INSERT INTO x VALUES ('a', 'tofu', 12418249)"
    pparse "INSERT INTO x (a,b,c) VALUES (12, 43, 'x')"

    pparse "CREATE TABLE y (a INT PRIMARY KEY, b VARCHAR(20) NOT NULL, c NUMERIC(3) UNIQUE)"

main = do
    args <- getArgs
    case args of
        []  -> mainX
        f:_ -> do
            x <- readFile f
            pparse x
