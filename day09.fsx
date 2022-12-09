type Vector =
    { X: int
      Y: int}

let vec x y = { X=x; Y = y}
let (++) v1 v2 =
    { X = v1.X + v2.X
      Y = v1.Y + v2.Y}

let (--) v1 v2 =
    { X = v1.X - v2.X
      Y = v1.Y - v2.Y}


let up = vec 0 1
let down = vec 0 -1
let left = vec -1 0
let right = vec 1 0 


type Rope =
    { Head: Vector
      Tail: Vector }

let start = { Head = vec 0 0; Tail = vec 0 0 }

let follow head tail  =
    let v = head -- tail
    if abs v.X <= 1 && abs v.Y <= 1 then
        // tail still in contact
        tail
    else
        // the head moved await in rope dir
        let u = vec (sign v.X) (sign v.Y)
        tail ++ u

let step dir rope =
    let head = rope.Head ++ dir
    let tail = follow head rope.Tail
    { Head = head
      Tail = tail
    }

let print rope =
    let builder = System.Text.StringBuilder()
    for y in 5 .. -1 .. 0 do
        for x in 0 .. 5 do
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

let input =
    """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2""".Split('\n')

let parse (line: string) =
    let parts = line.Split(' ')
    let dir =
        match parts[0] with
        | "U" -> up
        | "D" -> down
        | "L" -> left
        | "R" -> right
    dir, int parts[1]

System.IO.File.ReadAllLines("input/day09.txt")
|> Array.map parse
|> Seq.collect (fun (dir, count) -> seq { for i in 1.. count do dir } )
|> Seq.fold (fun (rope, visited) dir ->
    let rope = step dir rope  
    let visited = Set.add rope.Tail visited
    rope, visited
 ) (start, Set.empty)
|> snd |> Set.count


let printL rope =
    let builder = System.Text.StringBuilder()
    let len = List.length rope
    for y in 5 .. -1 .. 0 do
        for x in 0 .. 5 do
            match List.tryFindIndex (fun n -> n.X = x && n.Y = y) rope with
            | Some 0 -> builder.Append 'H'
            | Some n when n = len-1 -> builder.Append 'T'
            | Some n -> builder.Append n
            | None -> builder.Append '.'
            |> ignore
            
        builder.AppendLine "" |> ignore
    builder.ToString()
fsi.AddPrinter printL

let stepL dir longRope =
    longRope
    |> List.tail
    |> List.scan(fun prev next -> follow prev next) (List.head longRope ++ dir)

let startL n = [ for i in 1 .. n -> vec 0 0 ]
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
|> Seq.collect (fun (dir, count) -> seq { for _ in 1 .. count do dir})
|> Seq.fold (fun (rope, visited) dir ->
    let rope = stepL dir rope  
    let visited = Set.add (List.last rope) visited
    rope, visited
 ) (startL 10, Set.empty) 
|> snd |> Set.count