{-# LANGUAGE OverloadedStrings #-}
module Stratum.Page
    ( page, layout, PageType(..)
    , overview
    , byLocations, byGroups, byPersons, byUntagged
    , digestInfo
    ) where

import           Text.Blaze.Html5 hiding (map, Tag)
import           Text.Blaze.Html5.Attributes hiding (span, title, form)
import           Prelude hiding (head, div, span, id)
import qualified Prelude
import           Crypto.Hash (HashAlgorithm, Digest)
import           Data.Char (toLower)
import           Storage.HashFS (Tag(..), Category(..))

data PageType = Root | ByGroups | ByLocations | ByPersons | ByUntagged | PageOther
    deriving (Show,Eq)

page :: PageType -> Markup -> Html
page _ lyt = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "utf-8"
        meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        --  The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags
        meta ! name "description" ! content ""
        meta ! name "author" ! content ""
        link ! rel "icon" ! href (relativePath "favicon.ico")
        title "Stratum"
        --  Bootstrap core CSS
        link ! href (relativePath "css/bootstrap.min.css") ! rel "stylesheet"
        --  IE10 viewport hack for Surface/desktop Windows 8 bug
        --link ! href "css/ie10-viewport-bug-workaround.css" ! rel "stylesheet"
        --  Custom styles for this template
        link ! href (relativePath "css/dashboard.css") ! rel "stylesheet"
        --  Just for debugging purposes. Don't actually copy these 2 lines!
        -- [if lt IE 9]><script src="../../assets/js/ie8-responsive-file-warning.js"></script><![endif]
        --script ! src "js/ie-emulation-modes-warning.js" $ mempty
        --  HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries
        -- [if lt IE 9]>
        --       <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
        --       <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
        --     <![endif]
    body $ do
        _ <- lyt
        --  Bootstrap core JavaScript
        --     ==================================================
        --  Placed at the end of the document so the pages load faster
        script ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js" $ mempty
        script "window.jQuery || document.write('<script src=\"js/vendor/jquery.min.js\"></script>')"
        script ! src (relativePath "js/bootstrap.min.js") $ mempty
        --  Just to make our placeholder images work. Don't actually copy the next line!
        -- script ! src "js/vendor/holder.min.js" $ mempty
        --  IE10 viewport hack for Surface/desktop Windows 8 bug
        --script ! src "js/ie10-viewport-bug-workaround.js" $ mempty
        script ! src (relativePath "js/stratum.js") $ mempty

relativePath :: String -> AttributeValue
relativePath myPath = toValue ("/" `mappend` myPath)

layout :: PageType -> Html -> Markup
layout pty myContent = do
    nav ! class_ "navbar navbar-inverse navbar-fixed-top" $ div ! class_ "container-fluid" $ do
        div ! class_ "navbar-header" $ do
            button ! type_ "button" ! class_ "navbar-toggle collapsed" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" "#navbar" $ do
                span ! class_ "sr-only" $ "Toggle navigation"
                span ! class_ "icon-bar" $ mempty
                span ! class_ "icon-bar" $ mempty
                span ! class_ "icon-bar" $ mempty
            a ! class_ "navbar-brand" ! href "#" $ "Stratum"
        div ! id "navbar" ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! href "#" $ "Dashboard"
            li $ a ! href "#" $ "Settings"
            li $ a ! href "#" $ "Help"
    div ! class_ "container-fluid" $ div ! class_ "row" $ do
        div ! class_ "col-sm-3 col-md-2 sidebar" $ do
            ul ! class_ "nav nav-sidebar" $ do
                li ! activeClass Root $ a ! href "/" $ do
                    _ <- "Overview"
                    span ! class_ "sr-only" $ "(current)"
                li $ a ! href "#" $ "History"
            ul ! class_ "nav nav-sidebar" $ do
                li ! activeClass ByGroups $ a ! href (relativePath "by-groups/") $ "By Groups"
                li ! activeClass ByLocations $ a ! href (relativePath "by-locations/") $ "By Location"
                li ! activeClass ByPersons $ a ! href (relativePath "by-persons/") $ "By Persons"
                li ! activeClass ByUntagged $ a ! href (relativePath "untagged/") $ "Untagged"
            ul ! class_ "nav nav-sidebar" $ do
                li $ a ! href "" $ "Search 1"
                li $ a ! href "" $ "Search 2"
        div ! class_ "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" $ do
            myContent
  where
    activeClass expected
        | pty == expected = class_ "active"
        | otherwise       = class_ ""

overview :: Markup
overview = do
    div ! class_ "row" $ do
        --form ! class_ "" ! action "" $ do
        input ! type_ "text" ! id "search-input" ! class_ "form-control" ! placeholder "Search..."
        button ! type_ "submit" ! id "search-button" ! class_ "btn btn-success" $ (span ! class_ "glyphicon glyphicon-search" $ "")
    div ! class_ "row" $
        div ! class_ "col-sm-9" $
            div ! id "page-content" $
                span ""

printItem :: String -> String -> Html
printItem ty it = li $ a ! href (relativePath (ty ++ "/" ++ it ++ "/")) $ toHtml it

byStringItems :: String -> [String] -> Markup
byStringItems ty items =
    div ! class_ "row" $
        div ! class_ "col-sm-9" $
            div ! id "page-content" $
                ul $ mapM_ (printItem ty) items

byLocations :: [String] -> Markup
byLocations = byStringItems "location"

byGroups :: [String] -> Markup
byGroups = byStringItems "group"

byPersons :: [String] -> Markup
byPersons = byStringItems "person"

byUntagged :: HashAlgorithm h => [Digest h] -> Markup
byUntagged digests =
    div ! class_ "row" $
        div ! class_ "col-sm-9" $
            div ! id "page-content" $
                ul $ mapM_ printItems digests
  where
    printItems it = li $ a ! href (relativePath ("digest" ++ "/" ++ show it ++ "/")) $ toHtml (show it)
{-
        <input type="text" class="form-control" placeholder="2 BHK Flat, Pune Real Estate, Pest Control..." id="query" name="query" value="">
                   <div class="input-group-btn">
                  <button type="submit" class="btn btn-success"><span class="glyphicon glyphicon-search"></span></button>
                  </div>
                  -}

digestInfo :: HashAlgorithm h => Digest h -> [Tag] -> Markup
digestInfo digest tags =
    div ! class_ "row" $
        div ! class_ "col-sm-9" $
            div ! id "page-content" $ do
                p $ toHtml (show digest)
                ul $ mapM_ (\(Tag mcat n) -> printItem (map toLower $ show $ maybe Other Prelude.id mcat) n) tags
