{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), arr)
import Text.Pandoc (ParserState, WriterOptions)
import System.FilePath (takeBaseName)

import           Hakyll hiding (Page)
import qualified Hakyll as H

------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    -- Images and static files
    ["favicon.ico"]            --> copy
    ["img/**", "images/**"]    --> copy
    ["static/**", "files/**"]  --> copy
    ["js/**", "javascript/**"] --> copy

    -- CSS files
    ["css/*.css", "style/*.css", "stylesheets/*.css"] --> css

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
numRecentPosts = 3

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
    route $ setExtension "html"
    compile $ withFields postFields
        >>> applyTemplateCompiler "templates/post.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

postFields :: Compiler Page Page
postFields = arr (setIdentifier . setField "siteRoot" siteRoot)

setIdentifier :: Page -> Page
setIdentifier page = setField "id" identifier page
  where
    identifier = takeBaseName (getField "url" page)

-- Top-level pages
topLevel :: RulesM (Pattern Page)
topLevel = do
    route $ setExtension "html"
    compile $ withFields topLevelFields
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

-- Add the fields we need to top-level pages
topLevelFields :: Compiler Page Page
topLevelFields =
        setFieldPostList recentFirst "allPosts"
    >>> setFieldPostList (take numRecentPosts . recentFirst) "recentPosts"
    >>> setFieldPostList chronological "chronologicalPosts"

-- Create a post list based on ordering/selection
setFieldPostList :: ([Page] -> [Page]) -> String -> Compiler Page Page
setFieldPostList f k =
    setFieldPageList f "templates/post-item.html" k "posts/*"

------------------------------------------------------------------------
-- Utils

withFields :: Compiler Page Page -> Compiler Resource Page
withFields = pageCompilerWithFields parserState writerOptions id

parserState :: ParserState
parserState = defaultHakyllParserState

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
