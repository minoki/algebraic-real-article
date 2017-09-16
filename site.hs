--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.Pandoc.Options as PO
import qualified Data.Set as Set

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions
  where
    myReaderOptions = defaultHakyllReaderOptions { PO.readerExtensions = myReaderExtensions }
    myReaderExtensions = PO.Ext_tex_math_single_backslash `Set.insert` PO.readerExtensions defaultHakyllReaderOptions
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


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
    defaultContext
