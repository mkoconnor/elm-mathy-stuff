import Maybe
import Time
import Color
import Text
import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Window
import Mouse

positionDistance : (Int, Int) -> Float
positionDistance (x,y) = sqrt (toFloat (x * x + y * y))

mouseScaling : Signal Float
mouseScaling =
  let s =  Signal.foldp (\(isDown,position) (firstPositionWhenDown,overallScalingPlusSinceDown,overallScaling) ->
   let newFirstPos =
     if not isDown
     then Nothing
     else Just (Maybe.withDefault position firstPositionWhenDown)
   in
   let newOverallScaling =
     if not isDown
     then overallScalingPlusSinceDown
     else overallScaling
   in
   let newOverallScalingPlusSinceDown =
     if not isDown
     then overallScalingPlusSinceDown
     else
       case firstPositionWhenDown of
          Nothing -> overallScalingPlusSinceDown
          Just pos -> (positionDistance position / positionDistance pos) * overallScaling
   in
   (newFirstPos,newOverallScalingPlusSinceDown,newOverallScaling)) (Nothing,1,1) (Signal.map2 (\x y -> (x,y)) Mouse.isDown Mouse.position)
   in
   Signal.map (\(_,y,_) -> y) s

rotationsPerSecond = 1/4

type alias Model = { elapsedTime : Time.Time }

toElement : Model -> {width : Int, height : Int, scaling: Float} -> E.Element
toElement { elapsedTime } { width, height, scaling } =
  let minDim = min width height in
  let circleRadiusLength = 0.9 * scaling * toFloat minDim / 2 in
  let circle = C.outlined (C.solid Color.black) (C.circle circleRadiusLength) in
  let arcLength = 2 * pi * Time.inSeconds elapsedTime * rotationsPerSecond in
  let radius = C.traced (C.solid Color.black) (C.segment (0,0) (circleRadiusLength * cos arcLength, circleRadiusLength * sin arcLength)) in
  C.collage width height [circle, radius]
  
models : Signal Model
models = Signal.foldp (\timeSpan { elapsedTime } -> { elapsedTime = elapsedTime + timeSpan }) { elapsedTime = 0 } (Time.fps 60)

main = Signal.map3 (\model (width,height) scaling -> toElement model {width=width,height=height,scaling=scaling}) models Window.dimensions mouseScaling
