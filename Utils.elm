module Utils where
import Signal
import Random
import Time
type alias Signal a = Signal.Signal a

foldp : (a -> state -> state) -> Signal state -> Signal a -> Signal state
foldp f initial s = 
  let s' = Signal.foldp (\(initial,x) y ->
    let model = 
       case y of
          Nothing -> initial
          Just model -> model
     in
     Just (f x model)) Nothing (Signal.map2 (curry identity) initial s)
  in
  Signal.map2 (\initial y -> 
    case y of
      Nothing -> initial
      Just model -> model) initial s'

seedFromInitialTime : Signal Random.Seed
seedFromInitialTime = Signal.map (\(time,()) -> Random.initialSeed (round (Time.inMilliseconds time))) (Time.timestamp (Signal.constant ()))
