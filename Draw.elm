module Draw where
import Signal
import Graphics.Collage as C
import Graphics.Element as E
import Color
import List
import Mouse
import Window
import Maybe
import Time

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

sqr x = x * x

-- http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
sqrDistanceToSegment : ((Float,Float),(Float,Float)) -> (Float,Float) -> Float
sqrDistanceToSegment (v, w) p =
  let dist2 (vx, vy) (wx, wy) = sqr (vx - wx) + sqr (vy - wy) in
  let l2 = dist2 v w in
  if l2 == 0
  then dist2 p v
  else
    let
      t = ((fst p - fst v) * (fst w - fst v) + (snd p - snd v) * (snd w - snd v)) / l2
    in
    if | t < 0 -> dist2 p v
       | t > 1 -> dist2 p w
       | otherwise -> dist2 p (fst v + t * (fst w - fst v), snd v + t * (snd w - snd v))

listToPairs : List a -> List (a,a)
listToPairs l =
  case l of
    [] -> []
    _::[] -> []
    x::y::l' -> (x,y) :: listToPairs (y::l')

listToPairsWrapAround l =
  case l of
    [] -> []
    _::[] -> []
    x::_::_ ->
    let last = List.head (List.reverse l) in
    (last,x) :: listToPairs l

type alias Circle = { center : Point, radius : Float }

type alias Model = { drawn:List Point, firstPoint : Maybe Point, next:Point, closed:Bool, circles : List Circle }

type Update =
    Reset
  | Update { cursor:Point, clicked: Bool }

hitRadius = 5

legalNextPoint : Model -> Point -> Bool
legalNextPoint model point = 
  case model.drawn of
    [] -> True
    lastPoint :: earlierPoints -> 
      let collinearWithLastSegment =
        case earlierPoints of
           [] -> False
           secondLastPoint :: _ ->
              ccw lastPoint secondLastPoint point == Collinear
      in
      if collinearWithLastSegment
      then False
      else 
        let segments = listToPairs earlierPoints in
        let newSegment = (lastPoint, point) in
        not (List.any (intersects newSegment) segments)

biggestCircleAround : Model -> Point -> Circle
biggestCircleAround model point = 
  let toFloatPoint (x,y) = (toFloat x, toFloat y) in
  let allSegments = listToPairsWrapAround model.drawn in
  let minSegmentDistance = List.minimum (List.map (\(v,w) -> sqrDistanceToSegment (toFloatPoint v,toFloatPoint w) (toFloatPoint model.next)) allSegments) in
  let minCircleDistance = List.minimum (List.map (\circle ->
    let distToCenter = 
       sqrt (toFloat (sqr (fst circle.center - fst point) + sqr (snd circle.center - snd point)))
    in
    abs (distToCenter - circle.radius)) model.circles)
  in
  { center = point, radius = min (sqrt minSegmentDistance) minCircleDistance }

initialModel : Model
initialModel = {drawn = [], next = (0,0), closed = False, firstPoint = Nothing, circles = []}

updateModel : Update -> Model -> Model
updateModel update model = 
  case update of
     Reset -> initialModel
     Update update -> 
        if model.closed
        then { model | next <- update.cursor }
        else
        let model' = { model | next <- update.cursor } in
        if not update.clicked
        then model'
        else 
          if not (legalNextPoint model' update.cursor)
          then model'
          else 
            let shouldClose =
               case model'.firstPoint of
                  Nothing -> False
                  Just firstPoint ->
                    abs (fst update.cursor - fst firstPoint) < hitRadius && abs (snd update.cursor - snd firstPoint) < hitRadius
            in
            if shouldClose
            then { model' | closed <- True }
            else
              { drawn = update.cursor :: model'.drawn, next = update.cursor, closed = False, firstPoint = Just (Maybe.withDefault update.cursor model'.firstPoint), circles = [] }

toElement : Model -> {width : Int, height: Int} -> E.Element
toElement model { width, height } =
  let toFloatPoint (x,y) = 
    (toFloat x - (toFloat width)/2, (toFloat height)/2 - toFloat y)
  in
  let floatDrawn = List.map toFloatPoint model.drawn in
  let forms = 
    if model.closed
    then
      let allSegments = listToPairsWrapAround model.drawn in
      let minDistance = List.minimum (List.map (\(v,w) -> sqrt (sqrDistanceToSegment (toFloatPoint v,toFloatPoint w) (toFloatPoint model.next))) allSegments) in
      let circle = C.circle minDistance in
      let moveToPoint = C.move (toFloatPoint model.next) in
      [C.filled Color.lightBlue (C.polygon floatDrawn), C.traced (C.solid Color.black) (List.append floatDrawn [List.head floatDrawn]), moveToPoint (C.filled Color.lightGreen circle), moveToPoint (C.outlined (C.solid Color.black) circle)]
    else 
      let drawn = C.traced (C.solid Color.black) (C.path floatDrawn) in
      let next =
        case floatDrawn of
           [] -> []
           lastPoint :: _ ->
              let color =
                if legalNextPoint model model.next
                then Color.black
                else Color.red
              in
              [C.traced (C.dashed color) (C.segment lastPoint (toFloatPoint model.next))]
      in    
      let first =
         let circle point color = 
           C.move (toFloatPoint point) (C.filled color (C.circle (toFloat hitRadius)))
         in
         case model.firstPoint of
            Nothing -> circle model.next Color.gray
            Just firstPoint -> circle firstPoint Color.black
      in
      first :: drawn :: next
   in
   C.collage width height forms

port reset : Signal () 

updates : Signal Update
updates = Signal.merge (Signal.map (\() -> Reset) reset) (Signal.merge (Signal.map (\point -> Update {cursor=point, clicked = True}) (Signal.sampleOn Mouse.clicks Mouse.position)) (Signal.map (\point -> Update {cursor=point, clicked=False}) (Signal.sampleOn (Time.fps 60) Mouse.position)))

models : Signal Model
models = Signal.foldp updateModel initialModel updates 

main = Signal.map2 (\model (width, height) -> toElement model {width=width,height=height}) models Window.dimensions
