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

listToPairs : List a -> List (a,a)
listToPairs l =
  case l of
    [] -> []
    _::[] -> []
    x::y::l' -> (x,y) :: listToPairs (y::l')

type alias Model = { drawn:List Point, firstPoint : Maybe Point, next:Point, closed:Bool }

type alias Update = { cursor:Point, clicked: Bool }

hitRadius = 5

updateModel : Update -> Model -> Model
updateModel update model = 
  if model.closed
  then model
  else if not update.clicked
  then { model | next <- update.cursor }
  else 
    case model.drawn of
     [] -> { drawn = [update.cursor], next = update.cursor, closed = False, firstPoint = Just update.cursor }
     lastPoint :: earlierPoints ->
          let shouldClose =
             case model.firstPoint of
                Nothing -> False
                Just firstPoint ->
                  abs (fst update.cursor - fst firstPoint) < hitRadius && abs (snd update.cursor - snd firstPoint) < hitRadius
          in
          if shouldClose
          then { model | closed <- True }
          else
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
               else
                 { drawn = update.cursor :: model.drawn, next = update.cursor, closed = False, firstPoint = model.firstPoint }

toElement : Model -> {width : Int, height: Int} -> E.Element
toElement model { width, height } =
  let toFloatPoint (x,y) = 
    (toFloat x - (toFloat width)/2, (toFloat height)/2 - toFloat y)
  in
  let floatDrawn = List.map toFloatPoint model.drawn in
  let forms = 
    if model.closed
    then [C.filled Color.lightBlue (C.polygon floatDrawn)]
    else 
      let drawn = C.traced (C.solid Color.black) (C.path floatDrawn) in
      let next =
        case floatDrawn of
           [] -> []
           lastPoint :: _ -> [C.traced (C.dashed Color.black) (C.segment lastPoint (toFloatPoint model.next))]
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

updates : Signal Update
updates = Signal.merge (Signal.map (\point -> {cursor=point, clicked = True}) (Signal.sampleOn Mouse.clicks Mouse.position)) (Signal.map (\point -> {cursor=point, clicked=False}) (Signal.sampleOn (Time.every (10 * Time.millisecond)) Mouse.position))

models : Signal Model
models = Signal.foldp updateModel {drawn = [], next = (0,0), closed = False, firstPoint = Nothing} updates

main = Signal.map2 (\model (width, height) -> toElement model {width=width,height=height}) models Window.dimensions
