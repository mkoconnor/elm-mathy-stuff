import Html
import Html.Attributes
import Html.Events
import Html (Html)
import Text
import Graphics.Element as G
import Graphics.Element (Element)
import Array
import Array (Array)
import Random
import List
import Time
import Time (Time)
import Signal
import Debug

-- [dimension] is the common length of the rows and columns of [matrix]
type alias Model = { matrix : Array (Array Int), seed : Random.Seed, dimension : Int }

intGenerator = Random.int (-10) 10

dimension = 5

initialize : Random.Seed -> Model
initialize seed =
   let matrixRows rowsToProduce seed =
      if rowsToProduce == 0
      then ([], seed)
      else
        let
          (nextSeed, thisRow) =
             Random.generate (Random.list dimension intGenerator) seed
        in
        let (nextRows, finalSeed) = matrixRows (rowsToProduce - 1) nextSeed in
        (Array.fromList thisRow :: nextRows, finalSeed)
    in
    let (matrixRows', seed') = matrixRows dimension seed in
    { matrix = Array.fromList matrixRows', seed = seed', dimension = dimension }

type Update =
       Regenerate
     | FlipRow Int
     | FlipColumn Int

updates : Signal.Channel Update
updates = Signal.channel Regenerate

alignRight : Html.Attribute
alignRight = Html.Attributes.align "right"

toHtml : Model -> Html
toHtml m =
   let numberRows = Array.indexedMap m.matrix (\i row ->
      let htmlRow = List.map (\i -> 
            Html.td [alignRight] [Html.text (toString i)]) (Array.toList row)
      in
      let sum = Array.foldl (\i acc -> i + acc) 0 row in
      let sumTd = Html.td [alignRight, Html.Events.onClick (Signal.send updates (FlipRow i))] [Html.text (toString sum)] in
      Html.tr [] (List.append row [sumTd])
   )
   in
   let get_or_zero i j ar =
      -- the nothing cases shouldn't happen
      case Array.get i ar of
        Nothing -> 0
        Just row ->
         case Array.get j row of
           Nothing -> 0
           Just x -> x
   in
   let colSumsRow = Array.initialize m.dimension (\i ->
      let col = Array.initialize m.dimension (\j -> get_or_zero j i m.matrix) in
      let sum = Array.foldl (\i acc -> i + acc) 0 col in
      Html.td [alignRight, Html.Events.onClick (Signal.send updates (FlipColumn i))] [Html.text (toString sum)])
   in
   Html.table [] (List.append numberRows (Html.tr [] colSumsRow))      
      
timeAtStartOfProgram : Signal Time
timeAtStartOfProgram = Signal.map (\(time,()) -> time) (Time.timestamp (Signal.constant ()))

main : Signal Html
main = Signal.map (\time -> 
     toHtml (initialize (Random.initialSeed (round (Time.inMilliseconds time))))) timeAtStartOfProgram
