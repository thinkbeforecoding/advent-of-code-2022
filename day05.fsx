open System
let lines = IO.File.ReadAllLines("input/day05.txt")
let stacks = lines |> Array.takeWhile ((<>) "")

let nb =
    int (stacks[ ^0 ].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)[^0])


let parseLine nb (l: string) =
    [| for i in 0 .. nb - 1 do
           let c = l[i * 4 + 1]
           if c <> ' ' then Some c else None |]

let stack (stacks: (char list)[]) (line: char option[]) =
    [| for i in 0 .. stacks.Length - 1 do
           match line[i] with
           | Some x -> x :: stacks[i]
           | None -> stacks[i] |]

let start =
    lines
    |> Array.take (stacks.Length - 1)
    |> Array.rev
    |> Array.map (parseLine nb)
    |> Array.fold stack (Array.create nb [])


let moveLines = lines |> Array.skip (stacks.Length + 1)
let rx = System.Text.RegularExpressions.Regex "move (\d+) from (\d+) to (\d+)$"

let parseMove (l: string) =
    let m = rx.Match(l)
    let crates = int m.Groups[1].Value
    let from = int m.Groups[2].Value
    let dest = int m.Groups[3].Value
    crates, from - 1, dest - 1

let transfer (a: _[]) f (n, from, dest) =
    a
    |> Array.mapi (fun i v ->
        if i = from then a[from] |> List.skip n
        elif i = dest then f (List.take n a[from]) @ a[dest]
        else v)

let crane stacks move = transfer stacks List.rev move

let crane9001 stacks move = transfer stacks id move

let top (stacks: char list[]) = stacks |> Array.map List.head |> String

moveLines |> Seq.map parseMove |> Seq.fold crane start |> top

moveLines |> Seq.map parseMove |> Seq.fold crane9001 start |> top
