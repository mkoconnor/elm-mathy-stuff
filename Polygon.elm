import Window
import Text
import List
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input
import Signal
import Color

ngon : { elements:List E.Element, width:Int, height:Int } -> List C.Form
ngon args = 
  let maxdim = toFloat (min args.width args.height) / 2 in
  let ngonRadius = maxdim * 0.8 in
  let outerRadius = maxdim * 0.8 * 1.1 in
  let length = List.length args.elements in
  let withCoords = List.indexedMap (\i x ->
     let angle  = 2 * pi * (toFloat i)/(toFloat length) in
     let coords = (cos angle, sin angle) in
     (coords, x)) args.elements
  in
  let shape = C.polygon (List.map (\((x,y), _) -> (x * ngonRadius, y * ngonRadius)) withCoords)
  in
  let elements = List.map (\((x,y), element) -> 
    C.move (outerRadius * x, outerRadius * y) (C.toForm element)) withCoords
  in
  C.filled Color.lightBlue shape :: elements

main = Signal.map (\(width, height) -> C.collage width height (ngon { elements = List.map Text.asText [1,2,4,5,7,8,11], width = width, height = height })) Window.dimensions
