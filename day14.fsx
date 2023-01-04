// Day 14: Regolith Reservoir
open System

let input =
    """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

// first parse the input as lists of point
let parse (input: string) =
    input.Split('\n')
    |> Array.map (fun line ->
        line.Split(" -> ")
        |> Array.map (fun v ->
            let p = v.Split(',')
            int p[0], int p[1]))

let (++) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let (--) (x1, y1) (x2, y2) = x1 - x2, y1 - y2
let norm (x, y) = sign x, sign y

// returns coordinates of all points
let makeLines scan =
    let line =
        scan
        |> Array.pairwise
        |> Array.collect (fun (s, e) ->
            let dir = norm (e -- s)
            Array.unfold (fun c -> if c <> e then Some(c, c ++ dir) else None) s)

    Array.append line scan[^0..^0]

// prepares the map by filling all points
let drawMap floor points =
    let cave = Array.replicate (1000 * 200) '.'

    for (x, y) in points do
        cave[x + y * 1000] <- '#'

    if floor then
        let bottom = (points |> Seq.map snd |> Seq.max) + 2

        for x in 0..999 do
            cave[x + bottom * 1000] <- '#'

    cave

// get char in cave
let get (x, y) (cave: char[]) = cave[x + y * 1000]
// set a sand at given position
let setSand (x, y) (cave: char[]) =
    cave[x + y * 1000] <- 'o'
    cave

// print the cave
let print (x1, y1) (w, h) (cave: char[]) =
    for y in y1 .. y1 + h - 1 do
        for x in x1 .. x1 + w - 1 do
            printf "%c" cave[x + y * 1000]

        printfn ""

    cave
// direction vectors
let down = (0, 1)
let left = (-1, 1)
let right = (1, 1)

// moves sand down until falling in void or blocking at source
let rec sand p (cave: char[]) =
    if snd p >= 199 || get (500, 0) cave = 'o' then None
    elif get (p ++ down) cave = '.' then sand (p ++ down) cave
    elif get (p ++ left) cave = '.' then sand (p ++ left) cave
    elif get (p ++ right) cave = '.' then sand (p ++ right) cave
    else Array.copy cave |> setSand p |> Some

// fills new sand until blocked
let rec fill n cave =
    match sand (500, 0) cave with
    | None -> n
    | Some cave ->
        cave
        // |> print (450,0) (100,10)
        |> fill (n + 1)

// part 1
IO.File.ReadAllText "input/day14.txt"
|> parse
|> Array.collect makeLines
|> drawMap false
|> print (450, 0) (100, 12)
|> fill 0

// part 2
IO.File.ReadAllText "input/day14.txt"
|> parse
|> Array.collect makeLines
|> drawMap true
|> print (450, 0) (100, 12)
|> fill 0
