import Window
import Text
import List
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input as I
import Signal
import Color
import Array
import Random
import Utils
import Debug
type alias Array a = Array.Array a

type alias Model = { indices : Array Int, seed:Random.Seed }

type Update =
    FlipIndex Int
  | Regenerate

initialize : Random.Seed -> Model
initialize seed = 
  let (numVertices,seed') = Random.generate (Random.int 3 10) seed in
  let (vertices,seed'') = Random.generate (Random.list numVertices (Random.int (-10) 10)) seed' in
  let vertices' = 
     let sum = List.sum vertices in
     if | sum > 0  -> vertices
        | sum < 0  -> List.map (\x -> (-x)) vertices
        | otherwise -> List.map (\x -> x + 1) vertices
  in
  { indices = Array.fromList vertices', seed = seed'' }

mod : Int -> Int -> Int
mod i mod = 
  let ans = i % mod in
  if ans == mod then 0 else ans

updateModel : Update -> Model -> Model
updateModel update model = 
  case update of
    Regenerate -> initialize model.seed
    FlipIndex i -> 
     case Array.get i model.indices of
       Nothing -> model
       Just x  ->
         if x > 0
         then model
         else
           let 
              iPlusOne = mod (i + 1) (Array.length model.indices)
              iMinusOne = mod (i - 1) (Array.length model.indices)
           in            
           case Array.get iPlusOne model.indices of
              Nothing -> model
              Just iPlusOneVal -> 
                case Array.get iMinusOne model.indices of
                   Nothing -> model
                   Just iMinusOneVal -> 
                     let newIndices = Array.set iMinusOne (iMinusOneVal + x) (Array.set iPlusOne (iPlusOneVal + x) (Array.set i (-x) model.indices))
                     in
                     { model | indices <- newIndices }

updates : Signal.Channel Update
updates = Signal.channel Regenerate

toElement : Model -> {width:Int, height:Int} -> E.Element
toElement model dims = 
  let maxdim = toFloat (min dims.width dims.height) / 2 in
  let ngonRadius = maxdim * 0.8 in
  let outerRadius = maxdim * 0.8 * 1.1 in
  let length = Array.length model.indices in
  let withCoords = List.indexedMap (\i index ->
     let angle  = 2 * pi * (toFloat i)/(toFloat length) in
     let coords = (cos angle, sin angle) in
     let element = I.clickable (Signal.send updates (FlipIndex i)) (Text.asText index) in
     (coords, element)) (Array.toList model.indices)
  in
  let shape = C.polygon (List.map (\((x,y), _) -> (x * ngonRadius, y * ngonRadius)) withCoords)
  in
  let elements = List.map (\((x,y), element) -> 
    C.move (outerRadius * x, outerRadius * y) (C.toForm element)) withCoords
  in
  let button = I.button (Signal.send updates Regenerate) "Regenerate Polygon" in
  C.collage dims.width dims.height (C.filled Color.lightBlue shape :: C.toForm button :: elements)

models : Signal Model
models = Utils.foldp updateModel (Signal.map initialize Utils.seedFromInitialTime) (Signal.subscribe updates)

main = Signal.map2 (\(width, height) model -> toElement model {width = width, height = height }) Window.dimensions models
