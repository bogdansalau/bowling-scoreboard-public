module Model exposing (..)

import String exposing (fromInt)
type Frame = Strike | Spare Int | HalfFrame Int | OpenFrame Int Int
type alias FillBalls = (Maybe Int, Maybe Int)

type alias Model = {
  frameList: List Frame,
  fillBalls: FillBalls,
  lastFrame: Frame,
  isRoundComplete: Bool
  }

init: Model
init = emptyModel

frameToString: Frame -> String
frameToString frameResult = Debug.todo

fillBallToString: FillBalls -> String
fillBallToString fillBalls = Debug.todo

emptyModel = Debug.todo
