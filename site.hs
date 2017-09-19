--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.Pandoc.Options as PO
import qualified Data.Set as Set
import Control.Applicative (empty)

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


    postIDs <- getMatches "posts/*"
    let makeId pageNum = postIDs !! (pageNum - 1)
        grouper items = return (map (:[]) items) -- one item per group
    pag <- buildPaginateWith grouper "posts/*" makeId
    paginateRules pag $ \pageNum pattern -> do
        route $ setExtension "html"
        let pageTitle :: Int -> Compiler String
            pageTitle n | 1 <= n && n <= length postIDs = do
                            mtitle <- getMetadataField (makeId n) "title"
                            case mtitle of
                              Just title -> return title
                              Nothing -> empty
                        | otherwise = empty
            ctx = mconcat [ field "previousPageTitle" (\_ -> pageTitle (pageNum - 1))
                          , field "nextPageTitle" (\_ -> pageTitle (pageNum + 1))
                          , paginateContext pag pageNum
                          , postCtx
                          ]
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
    defaultContext
