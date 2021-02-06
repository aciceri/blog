{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main (main) where
import System.FilePath.Posix ( takeFileName
                             , takeDirectory
                             , takeBaseName
                             , takeExtension
                             , (</>)
                             )
import System.Environment ( lookupEnv )
import Hakyll
import Hakyll
import Hakyll.Web.Sass
import Text.Sass.Options ( SassOptions(..)
                         , defaultSassOptions
                         , SassOutputStyle(..)
                         )
import Hakyll.Images ( loadImage
                     , compressJpgCompiler
                     , ensureFitCompiler
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
    --, sassOutputStyle    = SassStyleCompressed
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
        >>= loadAndApplyTemplate "generator/templates/post.html" postContext
        >>= loadAndApplyTemplate "generator/templates/default.html" postContext

      depends <- makePatternDependency "generator/css/**.scss"
      rulesExtraDependencies [depends] $ do
        match (fromRegex "^generator/css/main.scss") $ do
          route $ stripRoute "generator/" `composeRoutes` setExtension "css"
          compile sassCompiler

    match "generator/katex/**" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/hyphenopoly/**" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/firacode/**.woff2" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/baskerville/**.woff" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "assets/images/**.jpg" $ version "large" $ do
      route $ stripRoute "assets/"
      compile $ loadImage
        >>= compressJpgCompiler 50
  
    match "assets/images/**.jpg" $ version "small" $ do
      route $ suffixRoute "small" `composeRoutes` stripRoute "assets/"
      compile $ loadImage
        >>= ensureFitCompiler 1200 600
        >>= compressJpgCompiler 90

    match ("assets/images/**" .&&. complement "**.jpg") $ do
      route $ stripRoute "assets/"
      compile $ copyFileCompiler

  
domain :: String
domain = "blog.ccr.ydns.eu"

root :: String
root = "https://" ++ domain

postContext :: Context String
postContext =
    dateField "date" "%Y-%m-%d" <>
    baseContext
  
baseContext :: Context String
baseContext = constField "item-type" "default"
  <> constField "root" root
  <> defaultContext


suffixRoute :: String -> Routes
suffixRoute suffix = customRoute makeSuffixRoute
  where
    makeSuffixRoute ident = parentDir </> suffixed  where
        p = toFilePath ident
        parentDir = takeDirectory p
        baseName = takeBaseName p
        ext = takeExtension p
        suffixed = baseName ++ "-" ++ suffix ++ ext

stripRoute :: String -> Routes
stripRoute txt = gsubRoute txt (const "")

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
