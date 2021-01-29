{-# LANGUAGE OverloadedStrings #-}

module Main(main) where
import Hakyll


main :: IO ()
main = hakyllWith config $ do
  match "templates/*" $
    compile templateBodyCompiler
  match "posts/**.org" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "./out",
      previewPort = 5000,
      providerDirectory = "./"
    }

root :: String
root = "https://ourblog.com"

postCtx :: Context String
postCtx =
    constField "root" root      <> -- here
    dateField "date" "%Y-%m-%d" <>
    defaultContext
