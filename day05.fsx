open System
let lines = IO.File.ReadAllLines("input/day05.txt")
let stacks =
    lines |> Array.takeWhile((<>)"")
let nb = int (stacks[ ^0].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)[^0])


let parseLine nb (l: string) =
    [| for i in 0 .. nb-1 do
        let c=l[i*4+1]  
        if c <> ' ' then
            Some c
        else
            None
     |]

let stack (stacks: (char list)[]) (line: char option []) =
    [| for i in 0 .. stacks.Length-1 do
        match line[i] with
        | Some x -> x :: stacks[i]
        | None -> stacks[i]
    |]

let start =
    lines
    |> Array.take (stacks.Length-1)
    |> Array.rev
    |> Array.map ( parseLine nb)
    |> Array.fold stack [|for i in 1..nb -> []|]


let moveLines = lines |> Array.skip(stacks.Length+1)
let rx = System.Text.RegularExpressions.Regex "move (\d+) from (\d+) to (\d+)$"
let parseMove (l: string) =
    let m = rx.Match(l)
    let crates = int m.Groups[1].Value
    let from = int m.Groups[2].Value
    let dest = int m.Groups[3].Value
    crates, from, dest

let rec crane (stacks: char list[]) (crates, from, dest) =
    if crates = 0 then
        stacks
    else
        let stackFrom = stacks[from-1]
        let crate = stackFrom.Head
        let stacks = Array.copy stacks
        stacks[from-1] <- stackFrom.Tail
        stacks[dest-1] <- crate::stacks[dest-1]
        crane stacks (crates-1, from, dest)

let  crane9001 (stacks: char list[]) (crates, from, dest) =
        let stackFrom = stacks[from-1]
        let crateStack = List.take crates stackFrom
        let stacks = Array.copy stacks
        stacks[from-1] <- List.skip crates stackFrom
        stacks[dest-1] <- crateStack @ stacks[dest-1]
        stacks
   

let top (stacks: char list[]) =
    [| for stack in stacks ->
        stack.Head
    |] |> String

moveLines
|> Seq.map parseMove
|> Seq.fold crane start
|> top

moveLines
|> Seq.map parseMove
|> Seq.fold crane9001 start
|> top
