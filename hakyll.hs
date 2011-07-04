{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc (ParserState, WriterOptions)

import Hakyll

main :: IO ()
main = hakyll $ do

    -- Images and static files
    ["favicon.ico"]           --> copy
    ["img/**", "images/**"]   --> copy
    ["static/**", "files/**"] --> copy
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

    -- Completely static.
    copy = route idRoute >> compile copyFileCompiler

    -- CSS directories
    css = route (setExtension "css") >> compile compressCssCompiler

    -- Templates
    template = compile templateCompiler

    -- Posts
    post = do
        route $ setExtension "html"
        compile $ pageCompilerWith parserState writerOptions
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Top-level pages
    topLevel = do
        route $ setExtension "html"
        compile $ pageCompilerWithFields parserState
            writerOptions id topLevelFields
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Add the fields we need to top-level pages
    topLevelFields = setFieldPostList recentFirst "allPosts"
        >>> setFieldPostList (take numRecentPosts . recentFirst) "recentPosts"
        >>> setFieldPostList chronological "chronologicalPosts"

    -- Create a post list based on ordering/selection
    setFieldPostList f k = setFieldPageList f
        "templates/post-item.html" k "posts/*"


numRecentPosts :: Int
numRecentPosts = 3

parserState :: ParserState
parserState = defaultHakyllParserState

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
    { feedTitle       = "Jacob Stanley :: IO Blog"
    , feedDescription = "Ceiling cat is watching you unsafePerformIO"
    , feedAuthorName  = "Jacob Stanley"
    , feedRoot        = "http://jacob.stanley.io"
    }
