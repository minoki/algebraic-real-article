module PandocUtil where
import Text.Pandoc
import qualified Text.Pandoc.Walk

isKanji :: Char -> Bool
isKanji c = (0x4E00 <= i && i <= 0x9FFF) -- CJK Unified Ideographs
            || (0xF900 <= i && i <= 0xFAFF) -- CJK Compatibility Ideographs
            || (0x3400 <= i && i <= 0x4DBF) -- CJK Unified Ideographs Extension A
            || (0x20000 <= i && i <= 0x2FA1F) -- CJK Unified Ideographs Extension B - F, Supplement
  where i = fromEnum c

transformRubyInString :: (String -> String -> Inline) -> String -> [Inline]
transformRubyInString makeSimpleRuby s = loop "" s
  where
    loop acc [] = prependRev acc []

    -- Ruby notation with explicit starting mark ('｜')
    -- e.g. "最小｜多項式《たこうしき》"
    loop acc s@('｜':xs)
      | (base,_:xs') <- break (== '《') xs
      , (read,_:rest) <- break (== '》') xs'
      = prependRev acc $ makeSimpleRuby base read : loop "" rest

      -- Since there is no matching '《 》', no need to worry for ruby text
      | otherwise = [Str (reverse acc ++ s)]

    -- Ruby notation without explicit starting mark
    -- e.g. "多項式《たこうしき》"
    loop acc@(a0:_) ('《':xs)
      | isKanji a0
      , (read,_:rest) <- break (== '》') xs
      , (revbase,acc') <- span isKanji acc
      = prependRev acc' $ makeSimpleRuby (reverse revbase) read : loop "" rest

    loop acc (x:xs) = loop (x:acc) xs

    prependRev s xs | null s = xs
                    | otherwise = Str (reverse s) : xs

-- TODO: Handle Meta
walkInlinesPandoc :: (Inline -> [Inline]) -> Pandoc -> Pandoc
walkInlinesPandoc f x = Text.Pandoc.Walk.walk (walkInlinesBlock f) x

walkInlinesInline :: (Inline -> [Inline]) -> Inline -> [Inline]
walkInlinesInline f x = walk x
  where
    walk x = f $ case x of
      Str xs -> x
      Emph xs -> Emph $ concatMap walk xs
      Strong xs -> Strong $ concatMap walk xs
      Strikeout xs -> Strikeout $ concatMap walk xs
      Superscript xs -> Superscript $ concatMap walk xs
      Subscript xs -> Subscript $ concatMap walk xs
      SmallCaps xs -> SmallCaps $ concatMap walk xs
      Quoted qt xs -> Quoted qt $ concatMap walk xs
      Cite ct xs -> Cite (map walkCitation ct) $ concatMap walk xs
      Code _ _ -> x
      Space -> x
      SoftBreak -> x
      LineBreak -> x
      Math _ _ -> x
      RawInline _ _ -> x
      Link attr xs target -> Link attr (concatMap walk xs) target
      Image attr xs target -> Image attr (concatMap walk xs) target
      Note xs -> Note (map (walkInlinesBlock f) xs)
      Span attr xs -> Span attr (concatMap walk xs)
    walkCitation (Citation id prefix suffix mode noteNum hash) = Citation id (concatMap walk prefix) (concatMap walk suffix) mode noteNum hash

walkInlinesBlock :: (Inline -> [Inline]) -> Block -> Block
walkInlinesBlock f x = walk x
  where
    walkInlines = concatMap (walkInlinesInline f) :: [Inline] -> [Inline]
    walkBlocks = map walk :: [Block] -> [Block]
    walk x = case x of
      Plain xs -> Plain $ walkInlines xs
      Para xs -> Para $ walkInlines xs
      CodeBlock _ _ -> x
      RawBlock _ _ -> x
      BlockQuote xs -> BlockQuote $ walkBlocks xs
      OrderedList attr body -> OrderedList attr $ map walkBlocks body
      BulletList body -> BulletList $ map walkBlocks body
      DefinitionList pairs -> DefinitionList $ [(walkInlines w, map walkBlocks d) | (w,d) <- pairs]
      Header i attr xs -> Header i attr $ walkInlines xs
      HorizontalRule -> x
      Table cap align colwidth headers rows -> Table cap align colwidth (map walkBlocks headers) (map (map walkBlocks) rows)
      Div attr body -> Div attr $ walkBlocks body
      Null -> x
