import Window
import Text
import List
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input
import Signal
import Color

ngon : { elements:List E.Element, ngonRadius:Float, outerRadius:Float } -> List C.Form
ngon args = 
  let length = List.length args.elements in
  let withCoords = List.indexedMap (\i x ->
     let angle  = 2 * pi * (toFloat i)/(toFloat length) in
     let coords = (cos angle, sin angle) in
     (coords, x)) args.elements
  in
  let shape = C.polygon (List.map (\((x,y), _) -> (x * args.ngonRadius, y * args.ngonRadius)) withCoords)
  in
  let elements = List.map (\((x,y), element) -> 
    C.move (args.outerRadius * x, args.outerRadius * y) (C.toForm element)) withCoords
  in
  C.filled Color.lightBlue shape :: elements

thisNgon = ngon { elements = List.map Text.asText [1,2,4,5,7,8,11], ngonRadius=100,outerRadius=110 }

main = Signal.map (\(width, height) -> C.collage width height thisNgon) Window.dimensions
