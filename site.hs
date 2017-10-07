--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (forM_)
import           Hakyll
import qualified Text.Pandoc.Options as PO
import qualified Data.Set as Set

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions
  where
    myReaderOptions = defaultHakyllReaderOptions { PO.readerExtensions = myReaderExtensions }
    myReaderExtensions = Set.union (PO.readerExtensions defaultHakyllReaderOptions)
                         $ Set.fromList [ PO.Ext_tex_math_single_backslash
                                        , PO.Ext_east_asian_line_breaks
                                        ]
    myWriterOptions = defaultHakyllWriterOptions { PO.writerHTMLMathMethod = PO.MathML Nothing }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    postIDs <- sortChronological =<< getMatches "posts/*"
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
          ctx = prevPageCtx `mappend` nextPageCtx `mappend` postCtx
      compile $ myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "週刊 代数的実数を作る" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%Y年 %-m月 %-d日" `mappend`
    modificationTimeField "modified" "%Y年 %-m月 %-d日" `mappend`
    defaultContext
