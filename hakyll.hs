{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (id)
import           Control.Arrow ((>>>), (>>^), (&&&), arr)
import           Control.Category (id)
import           Data.List (stripPrefix)
import           Data.Monoid (mappend)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Text.HTML.TagSoup
import           Text.Pandoc (ParserState, WriterOptions)
import           System.FilePath (takeBaseName)


import           Hakyll hiding (Page)
import qualified Hakyll as H

------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    -- Images and static files
    ["favicon.ico"] --> copy
    ["robots.txt"]  --> copy
    ["img/**"]      --> copy
    ["files/**"]    --> copy
    ["js/**"]       --> copy

    -- CSS files
    ["css/*.css"] --> css

    -- All templates
    ["templates/*"] --> template

    -- "Dynamic" content
    ["posts/*"] --> post

    -- Top-level pages
    ["*.markdown", "*.html", "*.rst", "*.lhs"] --> topLevel

    -- RSS
    match  "atom.xml" $ route idRoute
    create "atom.xml" $ requireAll_ "posts/*" >>> renderAtom feedCfg
  where
    -- Useful combinator here
    xs --> f = mapM_ (\p -> match p $ f) xs

numRecentPosts :: Int
numRecentPosts = 10

siteRoot :: String
siteRoot = "http://jacob.stanley.io"

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
    { feedTitle       = "Jacob Stanley :: IO Blog"
    , feedDescription = "Ceiling cat is watching you unsafePerformIO"
    , feedAuthorName  = "Jacob Stanley"
    , feedRoot        = siteRoot
    }

------------------------------------------------------------------------

type Page = H.Page String

-- Completely static.
copy :: RulesM (Pattern CopyFile)
copy = route idRoute >> compile copyFileCompiler

-- CSS directories
css :: RulesM (Pattern String)
css = route (setExtension "css") >> compile compressCssCompiler

-- Templates
template :: RulesM (Pattern Template)
template = compile templateCompiler

-- Posts
post :: RulesM (Pattern Page)
post = do
    route postRoute
    compile $ withFields postFields
        >>> applyTemplateCompiler "templates/post.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        >>> cleanUrlCompiler

postRoute :: Routes
postRoute = dropHeadDir & setExtension "html" & dateDirs & dirWithIndex
  where
    -- | drops the first directory, post/1/abc.html is routed to 1/abc.html
    dropHeadDir = gsubRoute "^[^/]+/" (const "")

    -- | takes a post named abc.html and routes it to abc/index.html
    dirWithIndex = gsubRoute "\\.html" (const "/index.html")

    -- | takes the first three numbers of a post and makes them in to
    -- directories, 2011-08-21-abc.html is routed to 2011/08/21/abc.html
    dateDirs  = numberDir & numberDir & numberDir
    numberDir = gsubRoute "^\\d+-" (\x -> init x ++ "/")

    (&) = composeRoutes

postFields :: Compiler Page Page
postFields = arr
    $ setIdentifier
    . setEscapedTitle
    . setCleanUrl
    . setField "siteRoot" siteRoot

setIdentifier :: Page -> Page
setIdentifier p = setField "id" identifier p
  where
    identifier = takeBaseName (getField "url" p)

setCleanUrl :: Page -> Page
setCleanUrl p = setField "url" url p
  where
    url = cleanUrl (getField "url" p)

setEscapedTitle :: Page -> Page
setEscapedTitle p = setField "escapedTitle" title p
  where
    title = replaceAll "'" (const "\\'") (getField "title" p)

-- Top-level pages
topLevel :: RulesM (Pattern Page)
topLevel = do
    route $ setExtension "html"
    compile $ withFields topLevelFields
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        >>> cleanUrlCompiler

-- Add the fields we need to top-level pages
topLevelFields :: Compiler Page Page
topLevelFields =
        setFieldPostList recentFirst "allPosts"
    >>> setFieldPostList (take numRecentPosts . recentFirst) "recentPosts"
    >>> setFieldPostList chronological "chronologicalPosts"

-- Create a post list based on ordering/selection
setFieldPostList :: ([Page] -> [Page]) -> String -> Compiler Page Page
setFieldPostList order key =
    setFieldPageList order "templates/post-item.html" key "posts/*"

------------------------------------------------------------------------
-- Utils

withFields :: Compiler Page Page -> Compiler Resource Page
withFields = pageCompilerWithFields parserState writerOptions id

parserState :: ParserState
parserState = defaultHakyllParserState

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

------------------------------------------------------------------------
-- URL mangling

cleanUrlCompiler :: Compiler Page Page
cleanUrlCompiler = arr $ fmap $ mapAttrs clean
  where
    clean (key, value)
      | key `S.member` urls = (key, cleanUrl value)
      | otherwise           = (key, value)

    urls = S.fromList ["src", "href"]

cleanUrl :: String -> String
cleanUrl url = fromMaybe url (stripSuffix "/index.html" url)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf xs = reverse `fmap` stripPrefix suf' xs'
  where
    suf' = reverse suf
    xs'  = reverse xs

mapAttrs :: (Attribute String -> Attribute String) -> String -> String
mapAttrs = mapTags . mapAttr

mapTags :: (Tag String -> Tag String) -> String -> String
mapTags f = renderTags . map f . parseTags

mapAttr :: (Attribute a -> Attribute a) -> Tag a -> Tag a
mapAttr f (TagOpen s a) = TagOpen s (map f a)
mapAttr _ x             = x
