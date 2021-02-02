{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main (main) where
import Hakyll
import Hakyll.Web.Sass
import Text.Pandoc.Options

import Data.Void
import Replace.Megaparsec
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "./out",
      previewPort = 5000,
      providerDirectory = "./"
    }

main :: IO ()
main = hakyllWith config $ do
  match "generator/templates/*" $
    compile templateBodyCompiler
  match "posts/**.org" $ do
    route $ setExtension "html"
    compile $ customCompiler
      >>= applyFilter embedYoutube
      >>= loadAndApplyTemplate "generator/templates/post.html" postCtx
      >>= loadAndApplyTemplate "generator/templates/default.html" postCtx

    depends <- makePatternDependency "generator/css/**.scss"
    rulesExtraDependencies [depends] $ do
      match (fromRegex "^generator/css/[^_].*.scss") $ do
        route $ setExtension "css"
        compile sassCompiler

domain :: String
domain = "blog.ccr.ydns.eu"

root :: String
root = "https://" ++ domain

postCtx :: Context String
postCtx =
    constField "root" root      <>
    dateField "date" "%Y-%m-%d" <>
    defaultContext

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
