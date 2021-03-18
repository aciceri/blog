{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main (main) where
import Fields
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
import           Data.List                      ( isPrefixOf
                                                , isSuffixOf
                                                )
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
    tags <- buildTags "posts/**.org" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            let ctx   = postContext tags

            route tagRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern

                let tagsCtx =
                        constField "title" title
                            <> listField "posts" ctx (return posts)
                            <> constField "tag" tag
                            <> constField "language" "en"
                            <> baseContext

                makeItem ""
                    >>= loadAndApplyTemplate "generator/templates/tag.html"     tagsCtx
                    >>= loadAndApplyTemplate "generator/templates/default.html" tagsCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls

    
    match "generator/templates/*" $
      compile templateBodyCompiler
  
    match "posts/**.org" $ do
      route $ postRoute
      compile $ customCompiler
        >>= saveSnapshot "posts-content"
        >>= applyFilter embedYoutube
        >>= loadAndApplyTemplate "generator/templates/post.html" (postContext tags)
        >>= loadAndApplyTemplate "generator/templates/default.html" (postContext tags)
        >>= relativizeUrls

      depends <- makePatternDependency "generator/css/**.scss"
      rulesExtraDependencies [depends] $ do
        match (fromRegex "^generator/css/custom.scss") $ do
          route $ stripRoute "generator/" `composeRoutes` setExtension "css"
          compile sassCompiler

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**.org"
            let ctx = postContext tags
            let archiveCtx = listField "posts" ctx (return posts)
                        <> publishedGroupField "years" posts ctx
                        <> constField "archive" "true"
                        <> constField "language" "en"
                        <> constField "title" "Archive"
                        <> baseContext

            makeItem ""
                >>= loadAndApplyTemplate "generator/templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "generator/templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls
                
    match "pages/home.org" $ do
            route $ constRoute "index.html"
            compile $ do
                posts <- recentFirst =<< loadAll "posts/**.org"
                let ctx = postContext tags
                let indexCtx =
                        listField "posts" ctx (return $ take 2 posts)
                            <> constField "home" "true"
                            <> constField "title" "Home"
                            <> constField "language" "en"
                            <> baseContext

                customCompiler
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "generator/templates/home.html" indexCtx
                    >>= loadAndApplyTemplate "generator/templates/default.html" indexCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls

    match "pages/contacts.org" $ do
            route $ constRoute "contacts/index.html"
            compile $ do
              let ctx = constField "title" "Contacts"
                        <> constField "contacts" "true"
                        <> constField "language" "en"
                        <> baseContext
              customCompiler
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "generator/templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "generator/js/custom.js" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match ("generator/katex/**" .&&. complement "**.md") $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/hyphenopoly/**" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/firacode/**.woff2" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "generator/mathjax-fonts/**.woff" $ do
      route $ stripRoute "generator/"
      compile $ copyFileCompiler

    match "assets/custom/**" $ do
      route $ idRoute
      compile $ copyFileCompiler

    match "assets/images/**.jpg" $ version "large" $ do
      route $ suffixRoute "original" `composeRoutes` stripRoute "assets/"
      compile $ copyFileCompiler
  
    match "assets/images/**.jpg" $ version "small" $ do
      route $ stripRoute "assets/"
      compile $ loadImage
        >>= ensureFitCompiler 768 600
        >>= compressJpgCompiler 90

    match ("assets/images/**" .&&. complement "**.jpg") $ do
      route $ stripRoute "assets/"
      compile $ copyFileCompiler

    create ["rss/rss.xml"] $ do
      route idRoute
      compile (feedCompiler renderRss)

    create ["atom/atom.xml"] $ do
      route idRoute
      compile (feedCompiler renderAtom)
  
domain :: String
domain = "blog.ccr.ydns.eu"

root :: String
root = "https://" ++ domain

postContext :: Tags -> Context String
postContext tags =  dateField "date" "%Y-%m-%d"
        <> allTagsField "tags" tags
        <> constField "item-type" "post"
        <> teaserField "teaser" "posts-content"
        <> peekField 50 "peek" "posts-content"
        <> readTimeField "read-time" "posts-content"
        <> pathField "sourcefile"
        <> versionField "git-commit" Commit
        <> versionField "git-commit-hash" Hash
        <> baseContext

baseContext :: Context String
baseContext = headVersionField "git-head-commit" Commit
                 <> headVersionField "git-head-commit-hash" Hash
                 <> headVersionField "git-head-commit-full" Full
                 <> constField "item-type" "default"
                 <> concatField "concat"
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
        suffixed = baseName ++ "~" ++ suffix ++ ext

stripRoute :: String -> Routes
stripRoute txt = gsubRoute txt (const "")

postRoute :: Routes
postRoute = gsubRoute ".org" (const "/index.html")

tagRoute :: Routes
tagRoute = gsubRoute ".html" (const "/index.html")

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


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "My blog"
    , feedDescription = "Cool description"
    , feedAuthorName  = "Andrea Ciceri"
    , feedAuthorEmail = "andrea.ciceri@autistici.org"
    , feedRoot        = "https://blog.ccr.ydns.eu"
    }

feedContext :: Context String
feedContext = bodyField "description"
        <> dateField "date" "%Y-%m-%d"
        <> baseContext

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedContext
  =<< fmap (take 10 ) . recentFirst
  =<< loadAllSnapshots "posts/**.org" "posts-content"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = let
    cleanIndex :: String -> String
    cleanIndex url
        | isSuffixOf idx url = take (length url - length idx) url
        | otherwise            = url
        where idx = "index.html"
    in return . fmap (withUrls cleanIndex)
