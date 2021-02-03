{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main (main) where
import System.Environment ( lookupEnv )
import Hakyll
import Hakyll.Web.Sass
import Text.Sass.Options ( SassOptions(..)
                         , defaultSassOptions
                         , SassOutputStyle(..)
                         )
import Text.Pandoc.Options

import Data.Void
import Replace.Megaparsec
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Maybe
  
config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "./out",
      previewPort = 5000,
      providerDirectory = "./"
    }

sassOptions :: Maybe FilePath -> SassOptions
sassOptions distPath = defaultSassOptions
    { sassSourceMapEmbed = True
    , sassOutputStyle    = SassStyleCompressed
    , sassIncludePaths   = fmap (: []) distPath
    }

main :: IO ()
main = do
  sassCompiler <- fmap (sassCompilerWith . sassOptions) (lookupEnv "THIRDPARTY")
  compilerEnv <- lookupEnv "HAKYLL_ENV"

  hakyllWith config $ do
    match "generator/templates/*" $
      compile templateBodyCompiler
    match "posts/**.org" $ do
      route $ postRoute
      compile $ customCompiler
        >>= applyFilter embedYoutube
        >>= loadAndApplyTemplate "generator/templates/post.html" postCtx
        >>= loadAndApplyTemplate "generator/templates/default.html" postCtx

      depends <- makePatternDependency "generator/css/**.scss"
      rulesExtraDependencies [depends] $ do
        match (fromRegex "^generator/css/[^_].*.scss") $ do
          route $ stripGeneratorRoute `composeRoutes` setExtension "css"
          compile sassCompiler

    match "generator/katex/**" $ do
      route $ stripGeneratorRoute
      compile $ copyFileCompiler

  
domain :: String
domain = "blog.ccr.ydns.eu"

root :: String
root = "https://" ++ domain

postCtx :: Context String
postCtx =
    constField "root" root      <>
    dateField "date" "%Y-%m-%d" <>
    defaultContext
    
stripGeneratorRoute :: Routes
stripGeneratorRoute = gsubRoute "generator/" (const "")

postRoute :: Routes
postRoute = gsubRoute ".org" (const "/index.html")

customCompiler :: Compiler (Item String)
customCompiler =
  let
    mathExtensions =
      [ Ext_tex_math_dollars
      , Ext_tex_math_double_backslash
      , Ext_latex_macros
      ]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr enableExtension defaultExtensions mathExtensions
    writerOptions =
      defaultHakyllWriterOptions
      { writerExtensions = newExtensions
      , writerHTMLMathMethod = MathJax ""
      }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions

applyFilter :: (Monad m, Functor f) => (String-> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str

embedYoutube text =
  let
    macro = do
      string "{yt:"
      id <- many alphaNumChar
      char '}'
      return id :: Parsec Void String String
    embed id = "<div class='youtube-wrapper'><iframe allowfullscreen='true' src='https://www.youtube.com/embed/" ++ id ++ "'></iframe></div>"
  in streamEdit macro embed text
