module Snake where

import Data.List

data Segment = Segment Int Int deriving (Eq)

newtype Snake = Snake [Segment]

data Board = Board Int Int

data Direction = UpD | DownD | LeftD | RightD deriving (Eq)

isSnakesHeadOnBoard (Board width height) (Snake (Segment x y : _)) =
  x >= 0 && y >= 0 && x <= width && y <= height

isSnakeNotEatsItself (Snake snake) = length snake == length (nub snake)

isGameContinues board snake =
  isSnakesHeadOnBoard board snake && isSnakeNotEatsItself snake

isMovePossible direction (Snake [s]) = True
isMovePossible direction (Snake (Segment headX headY:Segment neckX neckY : _)) =
  not (checkMove headX neckX headY neckY UpD DownD || checkMove headY neckY headX neckX LeftD RightD)
  where
    checkMove headA neckA headB neckB directInc directDec =
      let headEq = headA == neckA
          incMove = headB == neckB + 1 && direction == directInc
          decMove = headB == neckB - 1 && direction == directDec
      in headEq && (incMove || decMove)

moveSegment direction (Segment x y) =
  case direction of
    UpD    -> Segment x (y - 1)
    DownD  -> Segment x (y + 1) 
    LeftD  -> Segment (x - 1) y 
    RightD -> Segment (x + 1) y 

move direction snake@(Snake segments@(h : _)) =
  if isMovePossible direction snake
    then Snake (moveSegment direction h : init segments)
    else snake
