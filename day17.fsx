// Day 17: Pyroclastic Flow

open System

let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

let rocksDefs =
    [| [| "####" |]

       [| " # "; "###"; " # " |]

       [| "  #"; "  #"; "###" |]

       [| "#"; "#"; "#"; "#" |]

       [| "##"; "##" |] |]

type Rock =
    { Shape: string[]
      Width: int
      Height: int }

module Rock =
    let create shape =
        { Shape = shape
          Width = shape[0].Length
          Height = shape.Length }



type Chamber = { Tower: string list; Height: int64 }

module Chamber =
    let start = { Tower = [ "@@@@@@@" ]; Height = 0 }

    let isInLine n c = n >= -1L && n < c.Height
    let line n c = c.Tower[int (c.Height - 1L - n)]


type FallingRock = { Rock: Rock; X: int; Y: int64 }

module FallingRock =
    let lines r =
        [ r.Y + int64 r.Rock.Height - 1L .. -1L .. r.Y ]

    let line y r =
        r.Rock.Shape[int (r.Y + int64 r.Rock.Height - 1L - y)]

type Rocks = { Rocks: Rock[]; Index: int }


module Rocks =
    let start =
        { Rocks = rocksDefs |> Array.map Rock.create
          Index = 0 }

    let next r =
        { r with Index = (r.Index + 1) % r.Rocks.Length }

    let rock r = r.Rocks[r.Index]

type Input = { Moves: string; Index: int }

module Input =
    let create moves = { Moves = moves; Index = 0 }

    let move input = input.Moves[input.Index]

    let next (input: Input) =
        { input with Index = (input.Index + 1) % input.Moves.Length }

let appear chamber rocks =
    { Rock = Rocks.rock rocks
      X = 2
      Y = chamber.Height + 3L }

let fall r = { r with Y = r.Y - 1L }

let move input r =
    let d =
        match Input.move input with
        | '<' -> -1
        | '>' -> +1
        | _ -> failwith "bad input"

    { r with X = r.X + d |> max 0 |> min (7 - r.Rock.Width) }

let touch rock chamber =
    if rock.Y >= chamber.Height then
        false
    else
        FallingRock.lines rock
        |> Seq.exists (fun y ->
            Chamber.isInLine y chamber
            && (FallingRock.line y rock, Chamber.line y chamber |> Seq.skip rock.X)
               ||> Seq.exists2 (fun r c -> r = '#' && c <> ' '))

let addRock rock chamber =
    let rockLines =
        FallingRock.lines rock
        |> List.map (fun y ->
            if Chamber.isInLine y chamber then
                let l = Chamber.line y chamber |> Seq.toArray
                let r = FallingRock.line y rock

                for i in 0 .. rock.Rock.Width - 1 do
                    if r[i] = '#' then
                        l[i + rock.X] <- '#'

                String l
            else
                String(' ', rock.X)
                + FallingRock.line y rock
                + String(' ', 7 - rock.X - rock.Rock.Width)

        )

    let tower =
        seq {
            yield! Seq.truncate (max 0 (int (chamber.Height - rock.Y - int64 rock.Rock.Height))) chamber.Tower
            yield! rockLines
            yield! List.skip (int (chamber.Height - rock.Y)) chamber.Tower
        }
        |> Seq.toList


    { Tower = tower
      Height = max (rock.Y + int64 rock.Rock.Height) chamber.Height }


type Tetris =
    { Rocks: Rocks
      Input: Input
      Chamber: Chamber
      Rock: FallingRock
      Stopped: int64 }

let start input =
    { Rocks = Rocks.start
      Input = Input.create input
      Chamber = Chamber.start
      Rock = appear Chamber.start Rocks.start
      Stopped = 0L }

let step t =
    let lateral = t.Rock |> move t.Input

    let moved = if touch lateral t.Chamber then t.Rock else lateral
    let input = Input.next t.Input
    let next = fall moved

    if touch next t.Chamber then
        let rocks = Rocks.next t.Rocks
        let chamber = addRock moved t.Chamber

        { t with
            Chamber = chamber
            Rocks = rocks
            Input = input
            Rock = appear chamber rocks
            Stopped = t.Stopped + 1L }
    else
        { t with Rock = next; Input = input }

let step1 t =
    let lateral = t.Rock |> move t.Input

    let moved = if touch lateral t.Chamber then t.Rock else lateral

    { t with
        Rock = moved
        Input = Input.next t.Input }

let step2 t =
    let next = fall t.Rock

    if touch next t.Chamber then
        let rocks = Rocks.next t.Rocks
        let chamber = addRock t.Rock t.Chamber

        { t with
            Chamber = chamber
            Rocks = rocks
            Rock = appear chamber rocks
            Stopped = t.Stopped + 1L }
    else
        { t with Rock = next }

let draw t =
    Console.SetCursorPosition(0, 0)
    let line = Array.replicate 7 ' '
    let top = t.Chamber.Height + 5L
    let bottom = top - 20L

    for y in top .. -1L .. bottom do
        let f = if y < -1 then ' ' else '.'

        for i in 0..6 do
            line[i] <- f

        if Chamber.isInLine y t.Chamber then
            let s = Chamber.line y t.Chamber

            for i in 0 .. s.Length - 1 do
                if s[i] <> ' ' then
                    line[i] <- '#'

        if y = -1 then
            for i in 0..6 do
                line[i] <- '-'

        if y >= t.Rock.Y && y < t.Rock.Y + int64 t.Rock.Rock.Height then
            for x, c in Seq.indexed (FallingRock.line y t.Rock) do
                if c = '#' then
                    line[x + t.Rock.X] <- '@'

        let b =
            if y >= 0 then '|'
            elif y = -1 then '+'
            else ' '

        Console.Write(b)

        for c in line do
            Console.Write(c)

        Console.Write(b)
        Console.WriteLine()

    Console.WriteLine($"{t.Stopped} {Input.move t.Input}")

    Threading.Thread.Sleep 10
    t


let rec rund n t =
    if t.Stopped = n then
        draw t
    else
        draw t |> step1 |> draw |> step2 |> rund n

let rec run n t =
    if t.Stopped = n then t else t |> step |> run n

Console.CursorVisible <- false
Console.Clear()
input |> start |> rund 10 |> ignore
#time

let t =
    Console.Clear()
    // IO.File.ReadAllText("input/day17.txt")
    input |> start |> run 1000000L |> draw

1000000000000L / 1000000L
t.Chamber.Height


t.Chamber.Height
t.Chamber.Tower.Length

t.Chamber.Height

let rec findCycle s t =
    if List.contains (t.Input.Index, t.Rocks.Index) s then
        t, s
    else

        t |> step |> findCycle ((t.Input.Index, t.Rocks.Index) :: s)

let rec checkCycle n c t =
    if (t.Input.Index, t.Rocks.Index) = c then
        if n = 0 then t else t |> step |> checkCycle (n - 1) c
    else
        t |> step |> checkCycle n c

let t, s = IO.File.ReadAllText "input/day17.txt" |> start |> findCycle []

let c = (t.Input.Index, t.Rocks.Index)
let t = IO.File.ReadAllText "input/day17.txt" |> start |> checkCycle 0 c
let t2 = IO.File.ReadAllText "input/day17.txt" |> start |> checkCycle 1 c
let t3 = IO.File.ReadAllText "input/day17.txt" |> start |> checkCycle 2 c
let t4 = IO.File.ReadAllText "input/day17.txt" |> start |> checkCycle 3 c
let t5 = IO.File.ReadAllText "input/day17.txt" |> start |> checkCycle 4 c

draw t |> ignore
draw t2 |> ignore
draw t3 |> ignore
draw t4 |> ignore
draw t5 |> ignore

t3.Stopped - t2.Stopped


let cycleStart = int64 t2.Stopped
let heightStart = t2.Chamber.Height
let afterCycle = 1000000000000L - cycleStart
let cycleLen = int64 t3.Stopped - int64 t2.Stopped
let cycleCount = afterCycle / cycleLen
let cycleHeight = cycleCount * (t3.Chamber.Height - t2.Chamber.Height)
let remCount = afterCycle % cycleLen

let te = t2 |> run (t2.Stopped + remCount)
let h = te.Chamber.Height - t2.Chamber.Height


let total = heightStart + cycleHeight + h
t.Input.Index, t.Rocks.Index
List.findIndex ((=) (37, 3)) s
