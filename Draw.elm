import Signal
import Graphics.Collage as C
import Graphics.Element as E
import Color
import List
import Mouse
import Window

type alias Point = (Int, Int)

type Turn = 
  Clockwise
  | Counterclockwise
  | Collinear

-- http://algs4.cs.princeton.edu/91primitives/
-- [ccw A B C] determines the nature of the angle ABC
ccw : Point -> Point -> Point -> Turn
ccw (ax,ay) (bx,by) (cx,cy) = 
  let signedArea =
      (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)
  in
  if | signedArea > 0 -> Counterclockwise
     | signedArea < 0 -> Clockwise
     | otherwise      -> Collinear

oneDimIntersect : (Int, Int) -> (Int, Int) -> Bool
oneDimIntersect (x1,y1) (x2,y2) =
  let
     min1 = min x1 y1
     max1 = max x1 y1
     min2 = min x2 y2
     max2 = max x2 y2
  in
     min1 <= min2 && min2 <= max1
  || min1 <= max2 && max2 <= max1
  || min1 <= min2 && max2 <= max1
  || min2 <= min1 && max1 <= max2

type alias Segment = (Point, Point)
intersects : Segment -> Segment -> Bool
intersects (ap,aq) (bp, bq) =
  if (ccw ap aq bp /= ccw ap aq bq) && (ccw bp bq ap /= ccw bp bq aq)
  then True
  else
    let isCol p q r = ccw p q r == Collinear in
    let allCol = 
      isCol ap aq bp && isCol ap aq bq && isCol bp bq ap && isCol bp bq aq
    in
    if not allCol
    then False
    else
      oneDimIntersect (fst ap, fst aq) (fst bp, fst bq)
      && oneDimIntersect (snd ap, snd aq) (snd bp, snd bq)

listToPairs : List a -> List (a,a)
listToPairs l =
  case l of
    [] -> []
    _::[] -> []
    x::y::l' -> (x,y) :: listToPairs (y::l')

type alias Model = { drawn:List Point, next:Point }

type alias Update = { cursor:Point, clicked: Bool }
  
updateModel : Update -> Model -> Model
updateModel update model = 
  case model.drawn of
     [] -> if update.clicked
           then { drawn = [update.cursor], next = update.cursor }
           else { drawn = [], next = update.cursor }
     lastPoint :: earlierPoints ->
          let collinearWithLastSegment =
             case earlierPoints of
                [] -> False
                secondLastPoint :: _ ->
                   ccw lastPoint secondLastPoint update.cursor == Collinear
           in
           if collinearWithLastSegment
           then { model | next <- update.cursor }
           else 
             let segments = listToPairs earlierPoints in
             let newSegment = (lastPoint, update.cursor) in
             if List.any (intersects newSegment) segments
             then { model | next <- update.cursor }
             else { drawn = update.cursor :: model.drawn, next = update.cursor }

toFloatPoint : {width : Int, height: Int} -> Point -> (Float, Float)
toFloatPoint {width, height} (x,y) = 
  (toFloat x - (toFloat width)/2, (toFloat height)/2 - toFloat y)

toElement : Model -> {width : Int, height: Int} -> E.Element
toElement model { width, height } =
  C.collage width height [
    C.traced (C.solid Color.black) (C.path (List.map (toFloatPoint {width=width,height=height}) model.drawn))]

updates : Signal Update
updates = Signal.map (\point -> {cursor=point, clicked = True}) (Signal.sampleOn Mouse.clicks Mouse.position)

models : Signal Model
models = Signal.foldp updateModel {drawn = [], next = (0,0)} updates

main = Signal.map2 (\model (width, height) -> toElement model {width=width,height=height}) models Window.dimensions
