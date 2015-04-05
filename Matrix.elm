import Text
import Graphics.Element (Element)
import Array
import Array (Array)
import Random
import List
import Time
import Time (Time)
import Signal

type Model = Model { numbers : Array (Array Int) }

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

main : Element
main = Text.asText (initialize { seed = Random.initialSeed 1235, length = 5})
