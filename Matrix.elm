import Html
import Html.Attributes
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

type Model = Model { numbers : Array (Array Int) }

toHtml : Model -> Html
toHtml (Model r) =
    Html.table [] (
    List.map (\a ->
       Html.tr [] (
          List.map (\i -> Html.td [Html.Attributes.align "right"] [Html.text (toString i)])
           (Array.toList a)
        ))
     (Array.toList r.numbers))

toMatrix : { length:Int, list:List a } -> Array (Array a)
toMatrix r =
        let (acc, _) = 
         Array.foldl (\() (acc,l) ->
                   let first = List.take r.length l
                       rest = List.drop r.length l
                   in
                   ((Array.fromList first)::acc, rest)) ([],r.list)
                   (Array.initialize r.length (\_ -> ()))
        in
        Array.fromList acc

intGenerator = Random.int (-10) 10

initialize : { seed:Random.Seed, length: Int} -> Model
initialize r =
           let (numbers, _) = 
           Array.foldl (\() (m,seed) ->
              let (newVal,seed') = Random.generate intGenerator seed in
              (newVal::m, seed')
              ) ([],r.seed) (Array.initialize (r.length * r.length) (\_ -> ()))
           in
           Model { numbers = toMatrix { list = numbers, length = r.length } }

timeAtStartOfProgram : Signal Time
timeAtStartOfProgram = Signal.map (\(time,()) -> time) (Time.timestamp (Signal.constant ()))

main : Signal Html
main = Signal.map (\time -> 
     toHtml (initialize { seed = Random.initialSeed (round (Time.inMilliseconds time)), length = 5})) timeAtStartOfProgram
