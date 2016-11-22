module Main
    ( main
    ) where

import Data.SQL.Parse
import Data.SQL.Print
import System.Environment
import Control.Monad

pparse :: String -> IO ()
pparse str = do
    putStrLn "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
    case parse str of
        Right (stmts, [])      -> do
            putStrLn ("SUCCESS: " ++ str)
            putStrLn ("SUCCESS: ")
            forM_ stmts $ \stmt -> do
                putStrLn ("   " ++ show stmt)
                putStrLn ("   PRETTY: " ++ pretty stmt)
        Right (s, e@(l:_)) -> do
            putStrLn ("REMAINING: ")
            mapM_ (\v -> putStrLn ("  " ++ show v)) s
            putStrLn ("  => " ++ show e)
        Left err           -> putStrLn ("ERROR: " ++ show err)

mainX :: IO ()
mainX = do
    pparse "SELECT * FROM table"
    pparse "SELECT * FROM table; SELECT * from table"
    pparse "SELECT a,b FROM table WHERE a > 0"
    pparse "SELECT a,MEAN(b) FROM table WHERE a > 0"

    pparse "SELECT a,MEAN(b) FROM table WHERE a > 0 AND b > 0 OR b < 0 AND c > 0"

    pparse "SELECT d.given_on FROM   documents_document d WHERE  d.given_on >= '2001-01-01 0:0' AND    d.given_on <  '2001-02-01 0:0' ORDER  BY d.created_on DESC"

    pparse "INSERT INTO x VALUES ('a', 'tofu', 12418249)"
    pparse "INSERT INTO x (a,b,c) VALUES (12, 43, 'x')"
    pparse "INSERT INTO x VALUES ('a', E'\\x4a 2e 43 2e 52 2e 20 4c 69 63 6b 6c 69 64 65 72', 12418249)"

    pparse "UPDATE Customers SET ContactName='Alfred Schmidt', City='Hamburg' WHERE ContactName='x'"

    pparse "CREATE TABLE y (a INT PRIMARY KEY, b VARCHAR(20) NOT NULL, c NUMERIC(3) UNIQUE)"

main = do
    args <- getArgs
    case args of
        []  -> mainX
        f:_ -> do
            x <- readFile f
            pparse x
