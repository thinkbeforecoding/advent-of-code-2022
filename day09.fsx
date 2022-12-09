// simple integer vector arithmetic (manhattan style)
type Vector = { X: int; Y: int }

// ctor, addition, and subtraction
let vec x y = { X = x; Y = y }
let (++) v1 v2 = { X = v1.X + v2.X; Y = v1.Y + v2.Y }
let (--) v1 v2 = { X = v1.X - v2.X; Y = v1.Y - v2.Y }

// zero and unit vectors
let v0 = vec 0 0
let up = vec 0 1
let down = vec 0 -1
let left = vec -1 0
let right = vec 1 0

// the norm of the vector.. here we take just the largest coordinate
// So as long as to points are in the same 9 positions square, distance is <= 1
let norm v = max (abs v.X) (abs v.Y)

// kind of normalize the vector in the sens of the norm above
// since the resulting vector's cordinates are in -1/0/1
let normalize v = vec (sign v.X) (sign v.Y)

// single rope
type Rope = { Head: Vector; Tail: Vector }
let start = { Head = v0; Tail = v0 }

// gives the next position of the tail node
// given the new position of the head node
let follow head tail =
    let v = head -- tail // compute tension vector

    if norm v <= 1 then
        // tail still in contact to head node
        tail
    else
        // move at most a single unit on each axis
        tail ++ normalize v

// step in direction for a single move
let step dir rope =
    let head = rope.Head ++ dir
    let tail = follow head rope.Tail
    { Head = head; Tail = tail }

let print rope =
    let builder = System.Text.StringBuilder()

    for y in 5..-1..0 do
        for x in 0..5 do
            if x = rope.Head.X && y = rope.Head.Y then
                builder.Append 'H'
            elif x = rope.Tail.X && y = rope.Tail.Y then
                builder.Append 'T'
            else
                builder.Append '.'
            |> ignore

        builder.AppendLine "" |> ignore

    builder.ToString()

fsi.AddPrinter print

// parse an input line
let parse (line: string) =
    let parts = line.Split(' ')

    let dir =
        match parts[0] with
        | "U" -> up
        | "D" -> down
        | "L" -> left
        | "R" -> right

    dir, int parts[1]

// we know how to move 1 step, just replicate inputs dir
// to do all steps one by one
let flatten (dir, count) = Seq.replicate count dir

// Part 1

System.IO.File.ReadAllLines("input/day09.txt")
|> Array.map parse
|> Seq.collect flatten
|> Seq.fold
    (fun (rope, visited) dir ->
        let rope = step dir rope // make one step
        let visited = Set.add rope.Tail visited // mark visited position
        rope, visited)
    (start, Set.empty)
|> snd
|> Set.count

// Part 2
// now the rope is just a list of knot's positions
type LongRope = Vector list

let printL rope =
    let builder = System.Text.StringBuilder()
    let len = List.length rope

    for y in 5..-1..0 do
        for x in 0..5 do
            match List.tryFindIndex (fun n -> n.X = x && n.Y = y) rope with
            | Some 0 -> builder.Append 'H'
            | Some n when n = len - 1 -> builder.Append 'T'
            | Some n -> builder.Append n
            | None -> builder.Append '.'
            |> ignore

        builder.AppendLine "" |> ignore

    builder.ToString()

fsi.AddPrinter printL

// create rope of length n
let startL n : LongRope = List.replicate n v0

// step the full rope using the follow function
let stepL dir (rope: LongRope) : LongRope =
    // start moving the head in given direction
    // make each following nodes follow the previous ones
    rope.Tail |> List.scan follow (rope.Head ++ dir)

startL 10
|> stepL right
|> stepL right
|> stepL right
|> stepL right
|> stepL up
|> stepL up
|> stepL up
|> stepL up

System.IO.File.ReadAllLines("input/day09.txt")
|> Array.map parse
|> Seq.collect flatten
|> Seq.fold
    (fun (rope, visited) dir ->
        let rope = stepL dir rope
        let visited = Set.add (List.last rope) visited
        rope, visited)
    (startL 10, Set.empty)
|> snd
|> Set.count
