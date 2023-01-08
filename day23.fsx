// Day 23: Unstable Diffusion

open System

let input1 =
    """.....
..##.
..#..
.....
..##.
....."""

let input2 =
    """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."""


let parse (input: string) =
    let s = input.ReplaceLineEndings("")
    let w = input.IndexOfAny([| '\n'; '\r' |])
    let h = s.Length / w

    0,
    [ for y in 0 .. h - 1 do
          for x in 0 .. w - 1 do
              if s[x + y * w] = '#' then
                  (x, y) ]
    |> set


let (++) (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let N = (0, -1)
let NE = (1, -1)
let E = (1, 0)
let SE = (1, 1)
let S = (0, 1)
let SW = (-1, 1)
let W = (-1, 0)
let NW = (-1, -1)

let noElve p elves = Set.contains p elves |> not

let canGo dirs p elves =
    dirs |> List.fold (fun acc dir -> acc && noElve (p ++ dir) elves) true

let dontMove = canGo [ N; NE; E; SE; S; SW; W; NW ]

let canGoN = canGo [ NW; N; NE ]
let canGoE = canGo [ NE; E; SE ]
let canGoS = canGo [ SW; S; SE ]
let canGoW = canGo [ NW; W; SW ]

let tests = [| canGoN, N; canGoS, S; canGoW, W; canGoE, E |]

let getTests n =
    seq {
        for i in 0..3 do
            tests[(n + i) % 4]
    }


let elveDest n p elves =
    if dontMove p elves then
        p
    else
        getTests n
        |> Seq.tryPick (fun (canGo, dir) -> if canGo p elves then Some(p ++ dir) else None)
        |> Option.defaultValue p


let moveElves (n, elves) =
    let dests =
        elves
        |> Set.fold
            (fun m elve ->
                let d = elveDest n elve elves

                match Map.tryFind d m with
                | None -> Map.add d [ elve ] m
                | Some es -> Map.add d (elve :: es) m)
            Map.empty

    let newElves =
        dests
        |> Map.toSeq
        |> Seq.collect (fun (d, es) ->
            match es with
            | [ _ ] -> [ d ]
            | _ -> es)
        |> set

    n + 1, newElves

let rect elves =
    elves
    |> Seq.fold
        (fun (minx, miny, maxx, maxy) (x, y) -> min minx x, min miny y, max maxx x, max maxy y)
        (Int32.MaxValue, Int32.MaxValue, Int32.MinValue, Int32.MinValue)


let print (n, elves) =
    let (minx, miny, maxx, maxy) = rect elves
    let mutable n = 0

    for y in miny..maxy do
        for x in minx..maxx do
            if Set.contains (x, y) elves then
                printf "#"
            else
                printf "."
                n <- n + 1

        printfn ""

    printfn "Empty: %d" n


let rec moveElvesn n state =
    if n = 0 then
        state
    else
        state |> moveElves |> moveElvesn (n - 1)








input1 |> parse |> moveElvesn 3 |> print

input2 |> parse |> moveElvesn 10 |> print

IO.File.ReadAllText("input/day23.txt") |> parse |> moveElvesn 10 |> print

let rec run (n, elves) =
    let n', newElves = moveElves (n, elves)

    if newElves = elves then
        n'
    else
        printfn "%d: %d " n' (Set.count (newElves - elves))
        run (n', newElves)

input2 |> parse |> run

IO.File.ReadAllText("input/day23.txt") |> parse |> run
