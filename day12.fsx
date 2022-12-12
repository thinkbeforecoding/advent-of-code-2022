// Day 12: Hill Climbing Algorithm

let input =
    """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

type Map =
    { Map: string
      Width: int
      Height: int
      LineLength: int }

// replace S and E by 'a'-1 and 'z'+1
let startChar = char (int 'a' - 1)
let endChar = char (int 'z' + 1)

let parse (input: string) =
    let w = input.IndexOfAny([| '\n'; '\r' |])

    let l =
        if input[w + 1] >= 'a' && input[w + 1] <= 'z' then
            w + 1
        else
            w + 2

    let h = (input.Split('\n')).Length
    let m = input.Replace('S', startChar).Replace('E', endChar)

    { Map = m
      Width = w
      Height = h
      LineLength = l }

// find coordinates of S in map
let startPos map =
    let p = map.Map.IndexOf(startChar)
    let x = p % map.LineLength
    let y = p / map.LineLength
    (x, y)

// find coordinates of E in map
let endPos map =
    let p = map.Map.IndexOf(endChar)
    let x = p % map.LineLength
    let y = p / map.LineLength
    (x, y)

// get the value in the map at given coordinates
let value map (x, y) = int map.Map[x + y * map.LineLength]

let left (x, y) = x - 1, y
let right (x, y) = x + 1, y
let up (x, y) = x, y - 1
let down (x, y) = x, y + 1


// compute the manhattan distance between two points
let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let run start map =
    // indicates the distance from start of visited points (-1 => not visited)
    let visitedPoints = Array.create (map.Width * map.Height) -1
    let end' = endPos map
    let distFromStart (x, y) = visitedPoints[x + y * map.Width]
    // check if in out of map or visited
    let visited (x, y) =
        x < 0 || x >= map.Width || y < 0 || y >= map.Height || distFromStart (x, y) >= 0
    // set as visited with given distance form start
    let setvisited (x, y) d = visitedPoints[x + y * map.Width] <- d
    setvisited start 0

    // keep track of points to explore from.
    // prioritized by distance from start then distance to end
    let mutable toExplore = System.Collections.Generic.PriorityQueue()
    // begin exploring from start
    toExplore.Enqueue(start, (0, dist start end'))

    let rec move () =
        if toExplore.Count = 0 then
            // nothing more to explore, there is no path to E from start
            None
        else
            // find element nearest to end, not already explored
            let p = toExplore.Dequeue()
            let c = distFromStart p
            // get char at this point
            let s = value map p

            if s = int endChar then
                // this is the end char, found shortest path
                Some c
            else
                // explore in each direction
                for dir in [ left; right; up; down ] do
                    // move in direction
                    let n = dir p
                    // check if in map and not visited
                    if not (visited n) then
                        // get height at new location
                        let h = value map n
                        // check if path is at most 1 step up
                        if h <= s + 1 then
                            // mark point as visited with distance
                            setvisited n (c + 1)
                            // evaluate manhattan distance to optimize for straight path
                            let d = dist n end'
                            toExplore.Enqueue(n, (c + 1, d))

                // try next place to explore
                move ()

    move ()

let check =
    let map = parse input
    run (startPos map) map

let part1 =
    let sample = System.IO.File.ReadAllText("input/day12.txt")
    let map = parse sample
    map |> run (startPos map)

let part2 =
    let sample = System.IO.File.ReadAllText("input/day12.txt")
    let map = parse sample
    // find all start at height 'a'
    [ for y in 0 .. map.Height - 1 do
          for x in 0 .. map.Width - 1 do
              if value map (x, y) = int 'a' then
                  (x, y) ]
    |> List.choose (fun p -> run p map) // keep only those that have a path to end
    |> List.min // take the shortest one

(* Helper for debugging: prints the map sideway
for x in 0 .. map.Width-1 do
    for y in 0 .. map.Height-1 do
        let v = visitedPoints[x+y*map.Width]
        if value map (x,y) = int endChar then
            printf "  E "
        elif v < 0 then
            printf "  %c " (char (value map (x,y)))
        else
            printf "%3d%c" v (char (value map (x, y)))
    printfn ""
*)
