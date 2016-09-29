{-# LANGUAGE OverloadedStrings #-}

-- | Html sanitizer
module SanitizerNew where

import Control.Lens
import Control.Monad ((>=>))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Functor
import Data.List (intercalate, isInfixOf)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

-- data Tag str =
--      TagOpen str [Attribute str]  -- ^ An open tag with 'Attribute's in their original order
--    | TagClose str                 -- ^ A closing tag
--    | TagText str                  -- ^ A text node, guaranteed not to be the empty string
--    | TagComment str               -- ^ A comment
--    | TagWarning str               -- ^ Meta: A syntax error in the input file
--    | TagPosition !Row !Column     -- ^ Meta: The position of a parsed element
--      deriving (Show, Eq, Ord, Data, Typeable)
--
-- type Attribute str = (str,str)
--
-- data TagTree str
--     = TagBranch str
--                 [Attribute str]
--                 [TagTree str]
--     | TagLeaf (Tag str)
--     deriving (Eq,Ord,Show)
processTree
    :: Text -> Text
processTree =
    renderTree . modifyTree canProcess (toList . cleanTree) . parseTree

cleanTree :: TagTree Text -> Maybe (TagTree Text)
cleanTree =
    removeTag "script" >=>
    removeTag "style" >=> removeTag "iframe" >=> removeWS >=> removeSLink

modifyTree :: (TagTree s -> Bool)
           -> (TagTree s -> [TagTree s])
           -> [TagTree s]
           -> [TagTree s]
modifyTree descend act = concatMap f
  where
    f t@(TagBranch val attr sub)
      | descend t = act $ TagBranch val attr (modifyTree descend act sub)
      | otherwise = act t
    f x = act x

modifyTag
    :: (Monoid a, Eq a)
    => a -> (TagTree a -> Maybe (TagTree a)) -> TagTree a -> Maybe (TagTree a)
modifyTag name f t =
    if (t ^. ttname) == name -- TODO use case insensitive here
        then f t
        else Just t

removeWS :: TagTree Text -> Maybe (TagTree Text)
removeWS t =
    if (t ^. ttname & remove & T.null)
        then Nothing
        else Just t
  where
    remove = T.unwords . T.words

removeTag
    :: (Monoid a, Eq a)
    => a -> TagTree a -> Maybe (TagTree a)
removeTag name = modifyTag name (const Nothing)

removeSLink
    :: (Monoid a, Eq a, IsString a)
    => TagTree a -> Maybe (TagTree a)
removeSLink t =
    if link && stylesheet rel
        then Nothing
        else Just t
  where
    link = t ^. ttname == "link"
    rel = t ^. ttattr & lookup "rel"
    stylesheet (Just "stylesheet") = True -- TODO use case insensitive
    stylesheet _ = False

printTextTree :: Text -> IO ()
printTextTree = putStr . unpack

canProcess
    :: (Monoid a, Eq a, IsString a)
    => TagTree a -> Bool
canProcess t = not (n == "pre" || n == "code")
  where
    n = t ^. ttname

-- |
-- Lens for a tree tag name
-- > tag ^. ttname
-- > tag & ttname .~ "div"
ttname
    :: (Functor f, Monoid a)
    => (a -> f a) -> TagTree a -> f (TagTree a)
ttname f (TagBranch name a t) =
    (\n ->
          TagBranch n a t) <$>
    f name
ttname f (TagLeaf t) = TagLeaf <$> tname f t

-- |
-- Lens for attributes of a tree tag
-- > tag & ttattr %~ (fattr "href" $ const "/about.html")
-- > tag & ttattr %~ (sattr "href" $ const "/about.html")
-- > tag ^. ttattr & lookup "href"
ttattr
    :: Functor f
    => ([Attribute a] -> f [Attribute a]) -> TagTree a -> f (TagTree a)
ttattr f (TagBranch n attr t) =
    (\a ->
          TagBranch n a t) <$>
    f attr
ttattr f (TagLeaf t) = TagLeaf <$> tattr f t

-- |
-- Lens for a tag name
-- > tag ^. tname
-- > tag & tname .~ "div"
tname
    :: (Functor f, Monoid a)
    => (a -> f a) -> Tag a -> f (Tag a)
tname f (TagOpen name attrs) = flip TagOpen attrs <$> f name
tname f (TagClose name) = TagClose <$> f name
tname f (TagText name) = TagText <$> f name
tname f (TagComment name) = TagComment <$> f name
tname f (TagWarning name) = TagWarning <$> f name
tname f x@(TagPosition _ _) = x <$ void (f mempty)

-- |
-- Lens for attributes of a tag
-- > tag & tattr %~ (fattr "href" $ const "/home.html")
-- > tag & tattr %~ (sattr "href" $ const "/home.html")
-- > tag ^. tattr & lookup "href"
tattr
    :: Functor f
    => ([Attribute str] -> f [Attribute str]) -> Tag str -> f (Tag str)
tattr f (TagOpen name attr) = TagOpen name <$> f attr
tattr f t = t <$ void (f mempty)

-- |
-- Set the value of an attribute (overwrite or insert)
sattr
    :: Ord a
    => a -> a -> [Attribute a] -> [Attribute a]
sattr name value = Map.toList . (at name ?~ value) . Map.fromList

-- |
-- Modify the value of an attribute if it exists
fattr
    :: Ord a
    => a -> (a -> a) -> [Attribute a] -> [Attribute a]
fattr name f = Map.toList . (at name %~ fmap f) . Map.fromList