{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types hiding (parseQuery)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteArray as B (convert)

import Data.List

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString (fromByteString)
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import Storage.HashFS.Query
import Storage.HashFS.Meta
import Storage.HashFS hiding (Other)

import Crypto.Hash (HashAlgorithm)

import Stratum.Page

import System.FilePath
import System.Directory

data ContentType = Html | Css | Js | Svg | Tiff | Png | Other

--toCty :: ContentType -> AttributeValue
toCty :: ContentType -> B.ByteString
toCty (Html) = "text/html"
toCty (Css) = "text/css"
toCty (Js) = "text/javascript"
toCty (Svg) = "image/svg+xml"
toCty (Tiff) = "image/tif"
toCty (Png) = "image/png"
toCty Other = "application/octet-stream"

contentType :: ContentType -> ResponseHeaders
contentType ty =
    [("Content-Type", toCty ty)]

splitSlash :: B.ByteString -> [B.ByteString]
splitSlash rawPath =
    case B.break (== '/') rawPath of
        (b1,b2)
            | B.null b2 -> [b1]
            | otherwise -> b1 : splitSlash (B.drop 1 b2)

data GetQuery =
      GetCSS [B.ByteString]
    | GetJS [B.ByteString]
    | GetFonts [B.ByteString]
    | GetGroups
    | GetLocations
    | GetPersons
    | GetUntagged
    | GetGroup B.ByteString
    | GetLocation B.ByteString
    | GetPerson B.ByteString
    | GetDigest B.ByteString
    | GetRoot
    | FaviconIco
    deriving (Show,Eq)

doGet :: HashAlgorithm h => Context h -> Application
doGet ctx req respond = do
    let q = case pchunks of
            "":r@("css":_)          -> Just $ GetCSS r
            "":r@("js":_)           -> Just $ GetJS r
            "":r@("fonts":_)        -> Just $ GetFonts r
            "":"favicon.ico":[]     -> Just $ FaviconIco
            "":"by-groups":"":[]    -> Just $ GetGroups
            "":"by-locations":"":[] -> Just $ GetLocations
            "":"by-persons":"":[]   -> Just $ GetPersons
            "":"untagged":"":[]     -> Just $ GetUntagged
            "":"person":p:"":[]     -> Just $ GetPerson p
            "":"location":p:"":[]   -> Just $ GetLocation p
            "":"group":p:"":[]      -> Just $ GetGroup p
            "":"digest":p:"":[]      -> Just $ GetDigest p
            "":"":[]                -> Just $ GetRoot
            _                       -> Nothing
    case q of
        Nothing -> do
            putStrLn (show (requestMethod req) ++ " " ++ show (rawPathInfo req))
            respond $ responseLBS status404 [] "Requested thing not available"
        Just GetRoot      -> doSlash
        Just FaviconIco   -> respond $ responseLBS status404 [] "not found"
        Just (GetCSS r)   -> getFile Css r
        Just (GetJS r)    -> getFile Js r
        Just (GetFonts r) -> getFileAuto r
        Just GetGroups    -> doBy ByGroups byGroups Group
        Just GetLocations -> doBy ByLocations byLocations Location
        Just GetPersons   -> doBy ByPersons byPersons Person
        Just GetUntagged  -> doByDigests ByUntagged
        Just (GetGroup i) -> getByTag Group i
        Just (GetLocation i) -> getByTag Location i
        Just (GetPerson i) -> getByTag Person i
        Just (GetDigest d) -> getByDigest d
  where
    pchunks = splitSlash (rawPathInfo req)
    getByTag cat i = do
        let x   = htmlUnescape $ UTF8.toString i
            tag = Tag (Just cat) x
        let metaviders = contextMetaviders ctx
        digests <- metaFindDigestsWhich (head metaviders) (StructExpr $ DataTag $ StructExpr $ TagEqual tag)
        respondDigestList PageOther digests
    getByDigest d = do
        let x = htmlUnescape $ UTF8.toString d
        case inputDigestCtx ctx x of
            Nothing  -> respond $ responseLBS status500 (contentType Html) ("invalid digest " `L.append` L.fromStrict d)
            Just dig -> respondDigestPage dig
    doByDigests pty = do
        let metaviders = contextMetaviders ctx
        digests <- metaFindDigestsNotTagged (head metaviders)
        respondDigestList pty digests

    respondDigestPage digest = do
        let metaviders = contextMetaviders ctx
        tags <- metaDigestGetTags (head metaviders) digest
        respond $ responseBuilder status200 (contentType Html) (R.renderHtmlBuilder $ page PageOther (layout PageOther $ digestInfo digest tags))

    respondDigestList pty digests =
        respond $ responseBuilder status200 (contentType Html) (R.renderHtmlBuilder $ page pty (layout pty $ byUntagged digests))

    doBy pty rdr cat = do
        let metaviders = contextMetaviders ctx
        tags <- metaGetTags (head metaviders) (Just (StructExpr $ TagCat cat))
        let items = map unCategorize tags
        putStrLn (show pty ++ " : " ++ show (length items) ++ " items")
        respond $ responseBuilder status200 (contentType Html) (R.renderHtmlBuilder $ page pty (layout pty $ rdr items))
    doSlash = do
        putStrLn "/"
        respond $ responseBuilder status200 (contentType Html) (R.renderHtmlBuilder $ page Root (layout Root overview))

    getFile cty hp = do
        let fp = foldl (</>) "static" (map UTF8.toString hp)
        exist <- doesFileExist fp
        if exist
            then respond $ responseFile status200 (contentType cty) fp Nothing
            else putStrLn ("file " ++ show fp ++ " not found") >> respond (responseLBS status404 [] "not found")
    getFileAuto hp = do
        let fp = foldl (</>) "static" (map UTF8.toString hp)
        exist <- doesFileExist fp
        if exist
            then respond $ responseFile status200 (contentType Other) fp Nothing
            else putStrLn ("file " ++ show fp ++ " not found") >> respond (responseLBS status404 [] "not found")

doPost :: HashAlgorithm h => Context h -> Application
doPost ctx req respond = do
    m <- consumeBody [] 2048
    case m of
        Left err -> respond $ responseLBS status400 [] ("query error: " `mappend` L.fromStrict (UTF8.fromString err))
        Right query -> do
            let metaviders = contextMetaviders ctx
            digests <- metaFindDigestsWhich (head metaviders) query
            putStrLn ("digests found: " ++ show (length digests))
            let comma   = fromByteString $ B.pack ","
                builder = mconcat (intersperse comma $ map (fromByteString . B.pack . show) digests)
            respond $ responseBuilder status200 [] builder
  where
    consumeBody acc limit
        | limit < 0 = return $ Left "query over limit"
        | otherwise = do
            b <- requestBody req
            if B.null b
                then return $ parseQuery $ UTF8.toString $ mconcat $ reverse acc
                else consumeBody (b:acc) (limit - B.length b)

htmlUnescape :: String -> String
htmlUnescape []               = []
htmlUnescape ('%':'2':'0':xs) = ' ' : htmlUnescape xs
htmlUnescape ('%':'2':'f':xs) = '/' : htmlUnescape xs
htmlUnescape (x:xs)           = x : htmlUnescape xs

htmlEscape :: String -> String
htmlEscape [] = []
htmlEscape (x:xs)
    | x == ' '  = '%':'2':'0':htmlEscape xs
    | x == '/'  = '%':'2':'f':htmlEscape xs
    | otherwise = x:htmlEscape xs


app :: HashAlgorithm h => Context h -> Application
app ctx req respond = do
    case requestMethod req of
        "GET"  -> doGet ctx req respond
        "POST" -> doPost ctx req respond
        _      -> do
            putStrLn (show (requestMethod req) ++ " " ++ show (rawPathInfo req))
            respond $ responseLBS status500 [] "unknown method"

main :: IO ()
main = withConfig $ \ctx -> run 8080 (app ctx)
