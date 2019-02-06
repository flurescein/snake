module Snake where

import Data.List

data Segment = Segment Int Int deriving (Eq)

newtype Snake = Snake [Segment]

data Board = Board Int Int

data Direction = UpD | DownD | LeftD | RightD deriving (Eq)

isSnakesHeadOnBoard (Board width height) (Snake (Segment x y:_)) =
  x >= 0 && y >= 0 && x <= width && y <= height

isSnakeNotEatsItself (Snake snake) = length snake == length (nub snake)

isGameContinues board snake =
  isSnakesHeadOnBoard board snake && isSnakeNotEatsItself snake

isMovePossible direction (Snake [s]) = True
isMovePossible direction (Snake (Segment headX headY:Segment neckX neckY:_))
  | (headX == neckX && headY == neckY + 1 && direction == UpD)   ||
    (headX == neckX && headY == neckY - 1 && direction == DownD) ||
    (headY == neckY && headX == neckX + 1 && direction == LeftD) ||
    (headY == neckY && headX == neckX - 1 && direction == RightD) = False
  | otherwise = True

moveSegment direction (Segment x y) =
  case direction of
    UpD    -> Segment x (y - 1)
    DownD  -> Segment x (y + 1) 
    LeftD  -> Segment (x - 1) y 
    RightD -> Segment (x + 1) y 

move direction snake@(Snake segments@(h:_)) =
  if isMovePossible direction snake then 
    Snake (moveSegment direction h:init segments) 
  else 
    snake
