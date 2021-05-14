{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Control.Monad.Trans.State
import Data.Char

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Coordinate = Coordinate {value :: Int, maxValue :: Int} deriving (Eq, Show)

data Position = Position {x :: Coordinate, y :: Coordinate} deriving (Eq, Show)

data MoveDir = Forward | Backward

data TurnDir = TLeft | TRight

data Command = Move MoveDir | Turn TurnDir | None

newtype Rover = Rover (Position, Direction) deriving (Eq, Show)

data Result = Complete | NOK deriving Show

newtype Obstacle = Obstacle Position

rover :: Int -> Int -> Int -> Direction -> Rover
rover m x y d = Rover (Position (Coordinate x m) (Coordinate y m), d)

changeCoord :: MoveDir -> Coordinate -> Coordinate
changeCoord Forward (Coordinate v m) = Coordinate ((v + 1) `mod` (m + 1)) m
changeCoord Backward (Coordinate v m)
    | v > 0 = Coordinate (v - 1) m
    | otherwise = Coordinate m m

changePos :: MoveDir -> Position -> Direction -> Position
changePos Forward (Position x y) North = Position x (changeCoord Forward y)
changePos Forward (Position x y) East = Position (changeCoord Forward x) y
changePos Forward (Position x y) South = Position x (changeCoord Backward y)
changePos Forward (Position x y) West = Position (changeCoord Backward x) y
changePos Backward (Position x y) North = Position x (changeCoord Backward y)
changePos Backward (Position x y) East = Position (changeCoord Backward x) y
changePos Backward (Position x y) South = Position x (changeCoord Forward y)
changePos Backward (Position x y) West = Position (changeCoord Forward x) y

moveS :: Command -> State Rover Result
moveS = state . move
    where
        move (Move Forward) (Rover (p, d)) = (Complete, Rover (changePos Forward p d, d))
        move (Move Backward) (Rover (p, d)) = (Complete, Rover (changePos Backward p d, d))
        move (Turn TRight) (Rover (p, d)) = (Complete, Rover (p, csucc d))
        move (Turn TLeft) (Rover (p, d)) = (Complete, Rover (p, cpred d))
        move None r = (Complete, r)

translateCommands :: String -> [Command]
translateCommands = map f
  where
    f cmd = case toUpper cmd of
        'F' -> Move Forward
        'B' -> Move Backward
        'R' -> Turn TRight
        'L' -> Turn TLeft
        _ -> None

move :: String -> Rover -> Rover
move cmds = execState (mapM moveS (translateCommands cmds))


