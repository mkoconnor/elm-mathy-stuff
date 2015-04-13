import Time
import Color
import Text
import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Window

rotationsPerSecond = 1/4

type alias Model = { elapsedTime : Time.Time }

toElement : Model -> {width : Int, height : Int} -> E.Element
toElement { elapsedTime } { width, height } =
  let minDim = min width height in
  let circleRadiusLength = 0.9 * toFloat minDim / 2 in
  let circle = C.outlined (C.solid Color.black) (C.circle circleRadiusLength) in
  let arcLength = 2 * pi * Time.inSeconds elapsedTime * rotationsPerSecond in
  let radius = C.traced (C.solid Color.black) (C.segment (0,0) (circleRadiusLength * cos arcLength, circleRadiusLength * sin arcLength)) in
  C.collage width height [circle, radius]
  
models : Signal Model
models = Signal.foldp (\timeSpan { elapsedTime } -> { elapsedTime = elapsedTime + timeSpan }) { elapsedTime = 0 } (Time.fps 60)

main = Signal.map2 (\model (width,height) -> toElement model {width=width,height=height}) models Window.dimensions 
