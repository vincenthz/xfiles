module Data.Stream
    ( Stream(..)
    , eat
    , eatRet
    ) where

import Control.Applicative
import Data.Bifunctor

eatRet :: Show elem => (elem -> Maybe a) -> Stream elem a
eatRet predicate = Stream $ \el ->
    case el of
        []   -> Left ("empty stream: eating")
        x:xs ->
            case predicate x of
                Just a  -> Right (a, xs)
                Nothing -> Left ("unexpected atom got: " ++ show x ++ " next=" ++ show (take 5 xs))

eat :: Show elem => (elem -> Bool) -> Stream elem ()
eat predicate = Stream $ \el ->
    case el of
        [] -> Left ("empty stream: eating")
        x:xs
            | predicate x -> Right ((), xs)
            | otherwise   -> Left ("unexpected atom got: " ++ show x ++ " next=" ++ show (take 5 xs))

isEnd :: Stream elem Bool
isEnd = Stream $ \el -> Right (null el, el)

newtype Stream elem a = Stream { runStream :: [elem] -> Either String (a, [elem]) }

instance Functor (Stream elem) where
    fmap f s = Stream $ \e1 -> case runStream s e1 of
        Left err     -> Left err
        Right (a,e2) -> Right (f a, e2)
instance Applicative (Stream elem) where
    pure  = return
    fab <*> fa = Stream $ \e1 -> case runStream fab e1 of
        Left err      -> Left err
        Right (f, e2) -> either Left (Right . first f) $ runStream fa e2
instance Alternative (Stream elem) where
    empty     = Stream $ \_  -> Left "empty"
    f1 <|> f2 = Stream $ \e1 -> either (\_ -> runStream f2 e1) Right $ runStream f1 e1
instance Monad (Stream elem) where
    return a  = Stream $ \e1 -> Right (a, e1)
    ma >>= mb = Stream $ \e1 -> either Left (\(a, e2) -> runStream (mb a) e2) $ runStream ma e1
