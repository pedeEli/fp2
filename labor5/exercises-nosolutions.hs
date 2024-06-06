{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
--
import GHC.Exts (IsList, Item, fromList, fromListN, toList, IsString, fromString)
import Data.Char (toUpper)
--
data Team = TeamA | TeamB | TeamC | TeamD
  deriving (Eq, Show)
data Player = Player { name :: String
                     , team :: Team
                     , deaths :: Int
                     , points :: Int }
  deriving (Eq, Show)
--
player1 = Player { name = "Player One"
                  ,team = TeamA
                  ,deaths = 1
                  ,points = 5 }
--
player2 = Player { name = "Player Two"
                  ,team = TeamB
                  ,deaths = 4
                  ,points = 1 }
--
player3 = player1 { name = "Player Three", deaths = 2, points = 7 }
player4 = player2 { name = "Player Four", deaths = 4, points = 9 }
players = [player1, player2, player3, player4] :: [Player]
--
sorted :: Ord b => (a -> b) -> [a] -> [a]
sorted _ [] = []
sorted f (x:xs) = lhs ++ [x] ++ rhs
  where
    lhs = sorted f [y | y <- xs, f y <= f x]
    rhs = sorted f [y | y <- xs, f y > f x]
--
score :: Player -> Int
score ply = points ply - deaths ply
--
isBad :: Player -> Bool
isBad (score -> s) | s <= 0 = True
                   | otherwise = False
-- This function may help you in Exercise 1 a).
playersOfTeam :: Team -> [Player] -> [Player]
playersOfTeam t = filter (\ply -> t == team ply)
--
-- Exercise 1 a)
--
bestPlayerInTeam :: Team -> [Player] -> Maybe Player
bestPlayerInTeam team (sorted score . playersOfTeam team -> plys)
  | 0 <- length plys = Nothing
  | otherwise = Just $ last plys

-- This function may help you in Exercise 1 b).
unique :: Eq a => [a] -> [a]
unique = foldr (\x occ -> if x `elem` occ then occ else (x:occ)) []
-- These variables can help you test the different cases of `drawNumberOfTeams`.
player5 = player1 { name = "Player Five", deaths = 0, points = 0, team = TeamC }
player6 = player1 { name = "Player Six", deaths = 0, points = 0, team = TeamD }
oneTeamPlayers = filter (\ply -> team ply == TeamA) players
morePlayers = players ++ [player5, player6]
--
-- Exercise 1 b)
--
data GuiTeamsLayout = NoTeam | OneTeam | TwoTeams | ManyTeams
  deriving Show
--
drawNumberOfTeams :: [Player] -> GuiTeamsLayout
drawNumberOfTeams (unique . map team -> teams)
  | 0 <- length teams = NoTeam
  | 1 <- length teams = OneTeam
  | 2 <- length teams = TwoTeams
  | otherwise         = ManyTeams
--
-- Exercise 2 a)
--
(+-) :: Player -> Player
(+-) ply = ply {deaths = deaths ply + 1}
--
-- Exercise 3 a)
--
data PlayerList = PlayerList { teamA :: [Player]
                             , teamB :: [Player]
                             , teamC :: [Player]
                             , teamD :: [Player] }
  deriving Show
--
instance IsList PlayerList where
  type Item PlayerList = Player
  fromList :: [Item PlayerList] -> PlayerList
  fromList [] = PlayerList [] [] [] []
  fromList (ply:plys) = let list = fromList plys
    in case team ply of
      TeamA -> list {teamA = ply : teamA list}
      TeamB -> list {teamB = ply : teamB list}
      TeamC -> list {teamC = ply : teamC list}
      TeamD -> list {teamD = ply : teamD list}
  toList :: PlayerList -> [Item PlayerList]
  toList PlayerList {..} = teamA ++ teamB ++ teamC ++ teamD
-- Since the desugaring doesn't work in GHCI, you can instead evaluate this variable.
playersOverloaded = [player1, player2, player3, player4, player5, player6] :: PlayerList
--
-- Exercise 4 a)
--
data Language = English | German
  deriving Show
--
data LocalizedString (a :: Language) = LocalizedString String
--
instance Show (LocalizedString a) where
  show (LocalizedString s) = show s
--
-- Add your instances `IsString a` for `LocalizedString a` here.
instance IsString (LocalizedString a) where
  fromString :: String -> (LocalizedString a)
  fromString = LocalizedString
--
-- Remove undefined and uncomment the following lines
-- to test your instances for both languages.
--
someEnglishLocs = [
  "HELLO_world",
  "GAME_OVER",
  "PLAYER",
  "EXIT"] :: [LocalizedString English]
--
someGermanLocs = [
  "HELLO_world",
  "GAME_OVER",
  "PLAYER",
  "EXIT"] :: [LocalizedString German]