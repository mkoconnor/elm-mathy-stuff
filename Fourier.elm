import Maybe
import Time
import Color
import Text
import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Window
import Mouse
import Path

positionDistance : (Float, Float) -> Float
positionDistance (x,y) = sqrt (x * x + y * y)

scaledDimensions : Signal { width : Int, height : Int }
scaledDimensions = Signal.map (\(width, height) -> { width = round (toFloat width / 2), height = height } ) Window.dimensions

realMousePosition : Signal (Float, Float)
realMousePosition = Signal.map2 (\{width,height} (x,y) -> (toFloat x - toFloat width/2,toFloat height/2 - toFloat y)) scaledDimensions Mouse.position

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
   (newFirstPos,newOverallScalingPlusSinceDown,newOverallScaling)) (Nothing,1,1) (Signal.map2 (\x y -> (x,y)) Mouse.isDown realMousePosition)
   in
   Signal.map (\(_,y,_) -> y) s

rotationsPerSecond = 1/4

type alias Model = { elapsedTime : Time.Time, path : Path.Path, arcLength : Float, circleRadiusLength : Float }

initialModel : Model
initialModel = { elapsedTime = 0, path = Path.empty { timeToKeepPoints = 3 * Time.second }, arcLength = 0, circleRadiusLength = 0 }

currentPoint : Model -> (Float, Float)
currentPoint model =
 (model.circleRadiusLength * cos model.arcLength, model.circleRadiusLength * sin model.arcLength)

updateModel : Model -> { width : Int, height : Int, scaling : Float, timeSpan : Time.Time } -> Model
updateModel model { width, height, scaling, timeSpan } = 
  let minDim = min width height in
  let newCircleRadiusLength = 0.9 * scaling * toFloat minDim / 2 in
  let newElapsedTime = model.elapsedTime + timeSpan in
  let newArcLength = 2 * pi * Time.inSeconds newElapsedTime * rotationsPerSecond in
  let newModel = { model | circleRadiusLength <- newCircleRadiusLength, arcLength <- newArcLength, elapsedTime <- newElapsedTime }
  in
  let newPath = Path.pruneOld (Path.addPoint model.path { coords = currentPoint newModel, timeAdded = model.elapsedTime }) in
  { newModel | path <- newPath }
  
toElement : Model -> { width : Int, height : Int } -> E.Element
toElement model { width, height } = 
  let circle = C.outlined (C.solid Color.black) (C.circle model.circleRadiusLength) in
  let radius = C.traced (C.solid Color.black) (C.segment (0,0) (currentPoint model)) in
  let path = Path.toForm model.path in
  C.collage width height [circle, radius, path]

models : Signal Model
models = Signal.foldp (\(timeSpan, { width, height }, scaling) model -> updateModel model { width = width, height = height, scaling = scaling, timeSpan = timeSpan}) initialModel (Signal.map3 (\x y z -> (x,y,z)) (Time.fps 60) scaledDimensions mouseScaling)
  
main = Signal.map2 (\model {width, height} -> toElement model {width=width, height=height}) models scaledDimensions
