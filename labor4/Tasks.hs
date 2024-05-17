{-# LANGUAGE TemplateHaskell #-}
module Tasks where

import Control.Lens

-- | Exercise 0 | --
test = ((1, 0), 3, 4) & _1 . _2 .~ 2

todo :: a
todo = error "Not yet implemented"



-- | Exercise 1 | --
v = (1, ("Hi", "Ho"), 2)
-- a)
l :: Lens (Int, (a, String), Int) (Int, (b, String), Int) a b
l = _2 . _1
-- b)
partb = v ^. l
partb' = view l v
-- c)
vc = v & l .~ 42
vc' = set l 42 v
-- d)
vd = vc & l *~ 11
-- e)
v' = (["Hi", "Ho"], ["He", "Hu"])
-- This type is like Lens' ([String], [String]) String" with f = Const String
l' :: (String -> Const String String) -> ([String], [String]) -> Const String ([String], [String])
l' = _1 . _head
-- f)
-- This type is like Lens' ([String], [String]) String" with f = Const String
l'' :: (String -> Const String String) -> ([String], [String]) -> Const String ([String], [String])
l'' = _1 . ix 0
-- g)
-- This type is like Lens' ([String], [String]) String" but with an additional
-- Applicative constraint for "both".
l''' :: Applicative f => (String -> f String) -> ([String], [String]) -> f ([String], [String])
l''' = _1 . traverse
-- h)
vh = v' & l''' .~ "nice"



-- | Exercise 2 | --
data DocumentType = Text | PDF | Image
  deriving (Show, Eq)

data Metadata = Metadata {
    _title :: String,
    _author :: String
  } deriving Show

data Document = Doc {
    _docType :: DocumentType,
    _metadata :: Metadata,
    _content :: String
  } deriving Show

-- In good operating systems, "everything is a file"
-- We don't specify an inner type, as we later want to fold over it, but we
-- will only use "File Document"
data File a = Folder String [File a] | File a
  deriving Show

makePrisms ''DocumentType
makeLenses ''Document
makePrisms ''File

-- a)
-- makeLenses ''Metadata
title :: Lens Metadata Metadata String String
title = lens _title (\m t -> m {_title = t})
author :: Lens Metadata Metadata String String
author = lens _author (\m a -> m {_author = a})

-- b)
example :: File Document
example = Folder "root" [
    File $ Doc Text (Metadata ".zshenv" "root") "",
    Folder "home" [
        Folder "luke" [
            File $ Doc Text (Metadata ".zshenv" "luke") "export EDITOR=nvim",
            File $ Doc Text (Metadata ".zsh_history" "luke") "sudo dnf rm java"
          ]
      ]
  ]

-- c)
instance Semigroup Document where
  (<>) :: Document -> Document -> Document
  a <> _ = a
instance Monoid Document where
  mempty = Doc Text mempty ""
instance Semigroup Metadata where
  a <> _ = a
instance Monoid Metadata where
  mempty = Metadata "" ""

-- d)
temp = example ^? _File . metadata

-- e)
searchFiles :: String -> [Document] -> [Document]
searchFiles name = filter ((==) name . _title . _metadata)
-- f)
flattenFolders :: File Document -> [Document]
flattenFolders (File doc) = [doc]
flattenFolders (Folder _ files) = concatMap flattenFolders files
-- g)
searchFiles' :: String -> File Document -> [Document]
searchFiles' name = searchFiles name . flattenFolders

-- h)
instance Foldable File where
  foldr :: (a -> b -> b) -> b -> File a -> b
  foldr f init (File a) = f a init
  foldr f init (Folder _ files) = foldr (flip (foldr f)) init files
result = example ^.. folded

-- i)
result' = example ^.. folded . filtered (\doc -> _title (_metadata doc) == ".zshenv")

-- j)
searchFiles'' title = filtered $ (== title) . _title . _metadata
-- k)
searchAuthor author = filtered $ (== author) . _author . _metadata 
-- l)
filesWithAuthor title author = filtered $ \doc -> let m = _metadata doc in _title m == title && _author m == author



-- | Exercise 3 | --
-- a)
infixr 4 .~.
(.~.) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~.) f b = runIdentity . f (const $ Identity b)
-- b)
infixr 4 %~.
(%~.) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~.) f g = runIdentity . f (Identity . g)
-- c)
infixr 4 *~.
(*~.) :: Num a => ((a -> Identity a) -> s -> Identity t) -> a -> s -> t
(*~.) f n = runIdentity . f (Identity . (* n))
-- d)
infixr 4 .^.
(.^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
(.^.) s f = getConst $ f Const s
