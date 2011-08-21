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
main = hakyllWith configuration $ do

    -- Images and static files
    ["favicon.ico"] --> copy
    ["robots.txt"]  --> copy
    ["blank.html"]  --> copy
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
    create "atom.xml" $ requireAll_ "posts/*" >>> renderAtom feedConfiguration
  where
    -- Useful combinator here
    xs --> f = mapM_ (\p -> match p $ f) xs

numRecentPosts :: Int
numRecentPosts = 3

siteRoot :: String
siteRoot = "http://jacob.stanley.io"

configuration :: HakyllConfiguration
configuration = defaultHakyllConfiguration
    { deployCommand =
        "rsync -ave 'ssh' " ++ local ++ "/* " ++ server ++ ":" ++ remote
    }
  where
    server = "jystic_jacobstanley@ssh.phx.nearlyfreespeech.net"
    remote = "/home/public"
    local  = destinationDirectory defaultHakyllConfiguration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Jacob Stanley :: IO Blog"
    , feedDescription = "Ceiling cat is watching you unsafePerformIO"
    , feedAuthorName  = "Jacob Stanley"
    , feedRoot        = siteRoot
    }

------------------------------------------------------------------------

type Page = H.Page String

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
    $ setFieldFrom "url" "id" takeBaseName
    . setEscapedTitle
    . changeField "url" stripIndexPageName
    . setField "siteRoot" siteRoot
    . copyBodyToField "description"

setEscapedTitle :: Page -> Page
setEscapedTitle = setFieldFrom "title" "escapedTitle" escape
  where
    escape = replaceAll "'" (const "\\'")

------------------------------------------------------------------------

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
    >>> setFieldPostList chronological "chronologicalPosts"
    >>> setFieldPostTeasers (take numRecentPosts . recentFirst) "recentPosts"

-- Create a post list based on ordering/selection
setFieldPostList :: ([Page] -> [Page]) -> String -> Compiler Page Page
setFieldPostList order key =
    setFieldPageList order "templates/post-item.html" key "posts/*"

-- Create a post list based on ordering/selection
setFieldPostTeasers :: ([Page] -> [Page]) -> String -> Compiler Page Page
setFieldPostTeasers order key =
    setFieldPageList order "templates/post-teaser.html" key "posts/*"

------------------------------------------------------------------------
-- Utils

-- Completely static.
copy :: RulesM (Pattern CopyFile)
copy = route idRoute >> compile copyFileCompiler

-- CSS directories
css :: RulesM (Pattern String)
css = route (setExtension "css") >> compile compressCssCompiler

-- Templates
template :: RulesM (Pattern Template)
template = compile templateCompiler

withFields :: Compiler Page Page -> Compiler Resource Page
withFields = pageCompilerWithFields parserState writerOptions id

parserState :: ParserState
parserState = defaultHakyllParserState

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

-- | Maps a function over a field and stores it in a new field
setFieldFrom :: String -> String -> (String -> String) -> Page -> Page
setFieldFrom key key' f page = setField key' (f $ getField key page) page

------------------------------------------------------------------------
-- URL mangling

cleanUrlCompiler :: Compiler Page Page
cleanUrlCompiler = arr $ fmap $ mapAttrs clean
  where
    clean (key, value)
      | key `S.member` urls = (key, stripIndexPageName value)
      | otherwise           = (key, value)

    urls = S.fromList ["src", "href"]

stripIndexPageName :: String -> String
stripIndexPageName url = fromMaybe url (stripSuffix "/index.html" url)

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
