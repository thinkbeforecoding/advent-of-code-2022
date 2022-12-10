open System

type Instruction =
    | Noop
    | Addx of int

let parse line =
    match line with
    | "noop" -> Noop
    | _ -> line.Split(' ').[1] |> int |> Addx

type Proc = { X: int; Cycle: int }

let start = { X = 1; Cycle = 1 }

// wait one cycle
let wait proc = { proc with Cycle = proc.Cycle + 1 }
// add n to x at the end of the two cycles
let add n proc = { proc with X = proc.X + n }

// takes current proc and add each cycles to states
let eval instrs =
    let rec eval proc states instrs =
        match instrs with
        | Noop :: tail ->
            // on a nop, current state is added without change
            // and a cycle is added to move to next state
            let next = wait proc
            eval next (proc :: states) tail
        | Addx n :: tail ->
            let proc1 = wait proc
            let next = wait proc1 |> add n
            eval next (proc1 :: proc :: states) tail
        | [] -> List.rev states

    eval start [] instrs


let power p =
    if (p.Cycle - 20) % 40 = 0 then p.X * p.Cycle else 0


IO.File.ReadAllLines("input/day10.txt")
|> Seq.map parse
|> Seq.toList
|> eval
|> List.sumBy power

// part 2
// check if a pixel should be lit or not
let pixel proc =
    let pixel = (proc.Cycle - 1) % 40
    if abs (proc.X - pixel) <= 1 then '#' else '.'

IO.File.ReadAllLines("input/day10.txt")
|> Seq.map parse
|> Seq.toList
|> eval
|> List.map pixel
|> List.chunkBySize 40
|> List.iter (List.toArray >> String >> printfn "%s")


// ###..#....###...##..####.###...##..#....
// #..#.#....#..#.#..#.#....#..#.#..#.#....
// #..#.#....#..#.#..#.###..###..#....#....
// ###..#....###..####.#....#..#.#....#....
// #....#....#....#..#.#....#..#.#..#.#....
// #....####.#....#..#.#....###...##..####.
