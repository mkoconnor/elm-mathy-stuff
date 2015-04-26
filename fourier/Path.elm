module Path (Path, PointOnPath, empty, addPoint, toForm, pruneOld) where
import Time
import List
import Graphics.Collage as C
import Color

type alias PointOnPath = { coords : (Float, Float), timeAdded : Time.Time }

type alias Path = { points : List PointOnPath, timeToKeepPoints : Time.Time }

empty : { timeToKeepPoints : Time.Time } -> Path
empty { timeToKeepPoints } = { points = [], timeToKeepPoints = timeToKeepPoints }

addPoint : Path -> PointOnPath -> Path
addPoint { points, timeToKeepPoints } point =
   { points = point :: points, timeToKeepPoints = timeToKeepPoints }

pruneOld : Path -> Path
pruneOld path =
  case path.points of
     [] -> path
     lastPoint :: _ -> 
        let cutoff = lastPoint.timeAdded - path.timeToKeepPoints in
        let newPoints =
           List.filter (\point -> point.timeAdded >= cutoff) path.points
        in
        { points = newPoints, timeToKeepPoints = path.timeToKeepPoints }

defaultLine = C.defaultLine

toForm : Path -> C.Form
toForm path =
   C.traced { defaultLine | color <- Color.blue, width <- 3 } (C.path (List.map (\point -> point.coords) path.points))

