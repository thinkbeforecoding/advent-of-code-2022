// Day 20: Grove Positioning System

open System.IO

let input =
    """1
2
-3
3
-2
0
4"""
        .Split('\n')

let parse (input: string[]) = Array.map int64 input |> Array.indexed


let step n input =
    let pos = Array.findIndex (fun (n', _) -> n' = n) input
    let i = input[pos]
    let v = snd i
    let reduced = Array.removeAt pos input
    let l = Array.length reduced |> int64
    let newPos = ((int64 pos + v) % l + l) % l |> int
    let corrected = if v < 0L && newPos = 0 then int l else newPos
    Array.insertAt corrected i reduced

let rec run n input =
    if n = Array.length input then
        input
    else
        input |> step n |> run (n + 1)


let print input =
    printfn $"%A{Array.map snd input}"
    input

let coords input =
    let values = Array.map snd input
    let i = Array.findIndex ((=) 0L) values
    let l = Array.length values
    values[(i + 1000) % l] + values[(i + 2000) % l] + values[(i + 3000) % l]

let applyKey input =
    Array.map (fun (i, v) -> i, v * 811589153L) input

let rec runMulti n input =
    if n = 0 then input else input |> run 0 |> runMulti (n - 1)


input |> parse |> run 0 |> print |> coords

System.IO.File.ReadAllLines("input/day20.txt")
|> parse
|> run 0
|> print
|> coords


input |> parse |> applyKey |> runMulti 10 |> print |> coords


System.IO.File.ReadAllLines("input/day20.txt")
|> parse
|> applyKey
|> runMulti 10
|> print
|> coords
