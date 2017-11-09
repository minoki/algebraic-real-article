--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend,mconcat)
import           Data.Maybe (catMaybes)
import           Control.Monad (forM_,filterM)
import           Control.Applicative
import           Hakyll
import           Hakyll.Core.Configuration
import qualified Text.Pandoc as P
import           Text.Pandoc.XML (escapeStringForXML)
import qualified Data.Set as Set
import           System.Environment
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime
import           Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import           Data.Char (toUpper)
import qualified Network.URI as URI
import qualified PandocUtil

makeSimpleRuby :: String -> String -> P.Inline
makeSimpleRuby base read = P.RawInline (P.Format "html")
                           ("<ruby>" ++ escapeStringForXML base ++ "<rp>(</rp><rt>" ++ escapeStringForXML read ++ "</rt><rp>)</rp></ruby>")

myPandocCompiler :: P.HTMLMathMethod -> Compiler (Item String)
myPandocCompiler mathMethod = pandocCompilerWithTransform myReaderOptions myWriterOptions myTransform
  where
    myReaderOptions = defaultHakyllReaderOptions { P.readerExtensions = myReaderExtensions }
    myReaderExtensions = Set.union (P.readerExtensions defaultHakyllReaderOptions)
                         $ Set.fromList [ P.Ext_tex_math_single_backslash
                                        , P.Ext_east_asian_line_breaks
                                        ]
    myWriterOptions = defaultHakyllWriterOptions { P.writerHTMLMathMethod = mathMethod }
    myTransform :: P.Pandoc -> P.Pandoc
    myTransform = PandocUtil.walkInlinesPandoc transformInline
    transformInline :: P.Inline -> [P.Inline]
    transformInline (P.Str s) = PandocUtil.transformRubyInString makeSimpleRuby s
    transformInline x = [x]

isPublishMode :: IO Bool
isPublishMode = do
  mode <- lookupEnv "MODE"
  return ((map toUpper <$> mode) == Just "PUBLISH")

isDraft :: MonadMetadata m => Identifier -> m Bool
isDraft id = do
  f <- getMetadataField id "draft"
  return (f == Just "true")

encodeURIComponent :: [String] -> Item String -> Compiler String
encodeURIComponent args _item = return $ URI.escapeURIString URI.isUnreserved (concat args)

siteRoot :: String
siteRoot = "https://miz-ar.info/math/algebraic-real"

absoluteUrl :: Item a -> Compiler String
absoluteUrl item = do
  url <- maybe empty toUrl <$> getRoute (itemIdentifier item)
  return (siteRoot ++ stripIndexHtml url)
  where
    stripIndexHtml "/index.html" = "/"
    stripIndexHtml [] = []
    stripIndexHtml (x:xs) = x : stripIndexHtml xs

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle = "週刊 代数的実数を作る"
  , feedDescription = "代数的実数をプログラミング言語上で実装します。"
  , feedAuthorName = "ARATA Mizuki"
  , feedAuthorEmail = ""
  , feedRoot = "https://miz-ar.info/math/algebraic-real"
  }

makeExcerptField :: Item String -> Context String
makeExcerptField item = case splitAt 80 $ map replaceLineFeed $ stripTags $ itemBody item of
  (ex,[]) -> constField "excerpt" ex
  (ex,_rest) -> constField "excerpt" (ex ++ "\x2026") {- U+2026 HORIZONTAL ELLIPSIS -}
  where replaceLineFeed '\n' = ' '
        replaceLineFeed x = x

makeSeries :: [Identifier] -> (Context String -> Rules ()) -> Rules ()
makeSeries postIDs rule = do
  let nextPosts = tail $ map Just postIDs ++ [Nothing] :: [Maybe Identifier]
      prevPosts = Nothing : map Just postIDs           :: [Maybe Identifier]
  forM_ (zip3 postIDs nextPosts prevPosts) $ \(postID,mnextPost,mprevPost) -> create [postID] $ do
      let siblingCtx = mconcat $ catMaybes [(field "previousPageUrl"   . pageUrlOf)   <$> mprevPost
                                           ,(field "previousPageTitle" . pageTitleOf) <$> mprevPost
                                           ,(field "nextPageUrl"       . pageUrlOf)   <$> mnextPost
                                           ,(field "nextPageTitle"     . pageTitleOf) <$> mnextPost
                                           ]
      rule siblingCtx
        where
          pageTitleOf, pageUrlOf :: Identifier -> Item a -> Compiler String
          pageTitleOf i _item = do
            mtitle <- getMetadataField i "title"
            case mtitle of
              Just title -> return title
              Nothing -> fail "no 'title' field"
          pageUrlOf i _item = do
            mfilePath <- getRoute i
            case mfilePath of
              Just filePath -> return (toUrl filePath)
              Nothing -> fail "no route"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  publishMode <- isPublishMode
  tz <- getCurrentTimeZone
  let commonCtx, postCtx :: Context String
      useKaTeX = publishMode
      commonCtx =
        boolField "publish" (const publishMode) `mappend`
        boolField "use-katex" (const useKaTeX) `mappend`
        constField "katex-css" "https://static.miz-ar.info/katex-0.9.0-alpha/katex.min.css" `mappend`
        constField "katex-js" "https://static.miz-ar.info/katex-0.9.0-alpha/katex.min.js" `mappend`
        functionField "encodeURIComponent" encodeURIComponent `mappend`
        field "absoluteUrl" absoluteUrl
      postCtx =
        dateField "date" "%Y年%-m月%-d日" `mappend`
        localModificationTimeField tz "modified" "%Y年%-m月%-d日" `mappend`
        commonCtx `mappend`
        defaultContext

      mathMethod | useKaTeX = P.KaTeX "" ""
                 | not useKaTeX = P.MathML Nothing
      conf | not publishMode = Hakyll.Core.Configuration.defaultConfiguration
           | publishMode = Hakyll.Core.Configuration.defaultConfiguration
                           { destinationDirectory = "_site.pub"
                           , storeDirectory = "_cache.pub"
                           , tmpDirectory = "_cache.pub/tmp"
                           }
  hakyllWith conf $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let filterDraft | publishMode = filterM (\x -> not <$> isDraft x)
                    | not publishMode = return

    postIDs <- sortChronological =<< filterDraft =<< getMatches "posts/*"
    makeSeries postIDs $ \siblingCtx -> do
      route $ setExtension "html"
      let ctx = siblingCtx `mappend` postCtx
      compile $ myPandocCompiler mathMethod
        >>= saveSnapshot "content"
        >>= \item -> return item
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` makeExcerptField item)
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "週刊 代数的実数を作る" `mappend`
                    commonCtx `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bodyField "description" `mappend` postCtx
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

localModificationTimeField :: TimeZone -> String -> String -> Context a
localModificationTimeField = localModificationTimeFieldWith defaultTimeLocale

localModificationTimeFieldWith :: TimeLocale -> TimeZone -> String -> String -> Context a
localModificationTimeFieldWith locale tz key fmt = field key $ \i -> do
    mtimeUTC <- getItemModificationTime $ itemIdentifier i
    let mtimeLocal = utcToLocalTime tz mtimeUTC
    return $ formatTime locale fmt mtimeLocal
