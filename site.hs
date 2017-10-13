--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (forM_,filterM)
import           Hakyll
import qualified Text.Pandoc.Options as PO
import qualified Data.Set as Set
import           System.Environment
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime
import           Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions
  where
    myReaderOptions = defaultHakyllReaderOptions { PO.readerExtensions = myReaderExtensions }
    myReaderExtensions = Set.union (PO.readerExtensions defaultHakyllReaderOptions)
                         $ Set.fromList [ PO.Ext_tex_math_single_backslash
                                        , PO.Ext_east_asian_line_breaks
                                        ]
    myWriterOptions = defaultHakyllWriterOptions { PO.writerHTMLMathMethod = PO.MathML Nothing }

isDraftMode :: IO Bool
isDraftMode = do
  args <- getArgs
  return ("--draft" `elem` args)

isPublishMode :: IO Bool
isPublishMode = do
  args <- getArgs
  return ("--publish" `elem` args)

isDraft :: MonadMetadata m => Identifier -> m Bool
isDraft id = do
  f <- getMetadataField id "draft"
  return (f == Just "true")

--------------------------------------------------------------------------------
main :: IO ()
main = do
  draftMode <- isDraftMode
  tz <- getCurrentTimeZone
  let postCtx' = postCtx tz
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let filterDraft | draftMode = return
                    | not draftMode = filterM (\x -> not <$> isDraft x)

    postIDs <- sortChronological =<< filterDraft =<< getMatches "posts/*"
    let nextPosts = tail $ map Just postIDs ++ [Nothing]
        prevPosts = Nothing : map Just postIDs
    forM_ (zip3 postIDs nextPosts prevPosts)
      $ \(postID,mnextPost,mprevPost) -> create [postID] $ do
      route $ setExtension "html"
      let pageTitle, pageUrl :: Identifier -> Compiler String
          pageTitle i = do
            mtitle <- getMetadataField i "title"
            case mtitle of
              Just title -> return title
              Nothing -> fail "no 'title' field"
          pageUrl i = do
            mfilePath <- getRoute i
            case mfilePath of
              Just filePath -> return (toUrl filePath)
              Nothing -> fail "no route"
          prevPageCtx = case mprevPost of
                          Just i -> field "previousPageUrl"   (\_ -> pageUrl   i) `mappend`
                                    field "previousPageTitle" (\_ -> pageTitle i)
                          _ -> mempty
          nextPageCtx = case mnextPost of
                          Just i -> field "nextPageUrl"       (\_ -> pageUrl   i) `mappend`
                                    field "nextPageTitle"     (\_ -> pageTitle i)
                          _ -> mempty
          ctx = prevPageCtx `mappend` nextPageCtx `mappend` postCtx'
      compile $ myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx' (return posts) `mappend`
                    constField "title" "週刊 代数的実数を作る" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: TimeZone -> Context String
postCtx tz =
    dateField "date" "%Y年%-m月%-d日" `mappend`
    localModificationTimeField tz "modified" "%Y年%-m月%-d日" `mappend`
    defaultContext

localModificationTimeField :: TimeZone -> String -> String -> Context a
localModificationTimeField = localModificationTimeFieldWith defaultTimeLocale

localModificationTimeFieldWith :: TimeLocale -> TimeZone -> String -> String -> Context a
localModificationTimeFieldWith locale tz key fmt = field key $ \i -> do
    mtimeUTC <- getItemModificationTime $ itemIdentifier i
    let mtimeLocal = utcToLocalTime tz mtimeUTC
    return $ formatTime locale fmt mtimeLocal
