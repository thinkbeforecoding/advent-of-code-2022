// Day 22: Monkey Map
#r "nuget: FParsec"
open FParsec

let input =
    """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""
        .Split('\n')

let inputFaces = [| 8, 0; 0, 4; 4, 4; 8, 4; 8, 8; 12, 8 |]

let inputCnx =
    [| [| 5, 270; 3, 0; 2, 90; 1, 180 |]
       [| 2, 0; 4, 180; 5, 270; 0, 180 |]
       [| 3, 0; 4, 270; 1, 0; 0, 270 |]
       [| 5, 270; 4, 0; 2, 0; 0, 0 |]
       [| 5, 0; 1, 180; 2, 270; 3, 0 |]
       [| 0, 180; 1, 270; 4, 0; 3, 270 |] |]

type Move =
    | Advance
    | Left
    | Right

type State =
    { Map: string[]
      Moves: Move list
      Pos: int * int
      Dir: int * int
      Faces: (int * int)[]
      Cnx: (int * int)[][]
      Width: int }

let pMove =
    (pchar 'L' >>% [ Left ])
    <|> (pchar 'R' >>% [ Right ])
    <|> (pint32 |>> (fun n -> List.replicate n Advance))

let parseMoves line =
    match run (many pMove) line with
    | Success(v, _, _) -> v |> List.concat
    | Failure(m, _, _) -> failwith m


let get (x, y) state =
    if y < 0 || y >= Array.length state.Map then
        ' '
    else
        let l = state.Map[y]
        if x < 0 || x >= l.Length then ' ' else l[x]






let parse f (input: string[]) : State =
    let map = input[0..^2]
    let moves = parseMoves input[^0]
    let x = map[ 0 ].IndexOf('.')

    { Map = map
      Moves = moves
      Pos = x, 0
      Dir = 1, 0
      Faces = [||]
      Cnx = [||]
      Width = 0 }
    |> f

let (++) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let (--) (x1, y1) (x2, y2) = x1 - x2, y1 - y2

let left (x, y) = y, -x
let right (x, y) = -y, x

let advance state =
    let nextPos = state.Pos ++ state.Dir
    let v = get nextPos state
    let x, y = state.Pos
    let dx, dy = state.Dir

    if v <> ' ' then
        nextPos
    else
        let x =
            if dx > 0 then
                state.Map[ y ].IndexOfAny([| '.'; '#' |])
            elif dx < 0 then
                state.Map[ y ].LastIndexOfAny([| '.'; '#' |])
            else
                x

        let y =
            if dy > 0 then
                state.Map
                |> Array.findIndex (fun l -> x < l.Length && (l[x] = '.' || l[x] = '#'))
            elif dy < 0 then
                state.Map
                |> Array.findIndexBack (fun l -> x < l.Length && (l[x] = '.' || l[x] = '#'))
            else
                y

        x, y






let move state =
    match state.Moves with
    | [] -> state
    | Left :: rest ->
        let dx, dy = state.Dir

        { state with
            Dir = left state.Dir
            Moves = rest }
    | Right :: rest ->
        let dx, dy = state.Dir

        { state with
            Dir = right state.Dir
            Moves = rest }
    | Advance :: rest ->
        let next = advance state

        match get next state with
        | '.' -> { state with Pos = next; Moves = rest }
        | '#' -> { state with Moves = rest }
        | _ -> failwith "unexpected"


let rec run state =
    match state.Moves with
    | [] -> state
    | _ -> move state |> run

let password state =
    let x, y = state.Pos
    let row = y + 1
    let col = x + 1

    let facing =
        match state.Dir with
        | 1, 0 -> 0
        | 0, 1 -> 1
        | -1, 0 -> 2
        | 0, -1 -> 3
        | _ -> failwith "unknown dir"

    1000 * row + 4 * col + facing


input |> parse id |> run |> password

System.IO.File.ReadAllLines("input/day22.txt") |> parse id |> run |> password

(*
F0 => > F1 0
      v F2 0
      < F4 180
      ^ F5 +90
F1 => > F3 180
      v F2 +90
      < F0 0
      ^ F5 0
F2 => > F1 +90
      v F3 0
      < F4 +90
      ^ F0 0
F3 => > F1 180
      v F5 +90
      < F4 0
      ^ F2 0
F4 => > F3 0
      v F5 0
      < F0 180
      ^ F2 +90
F5 => > F3 +90
      v F1 0
      < F0 +90
      ^ F4 0
*)

let faces = [| 50, 0; 100, 0; 50, 50; 50, 100; 0, 100; 0, 150 |]

let cnx =
    [| [| 1, 0; 2, 0; 4, 180; 5, 270 |]
       [| 3, 180; 2, 270; 0, 0; 5, 0 |]
       [| 1, 90; 3, 0; 4, 90; 0, 0 |]
       [| 1, 180; 5, 270; 4, 0; 2, 0 |]
       [| 3, 0; 5, 0; 0, 180; 2, 270 |]
       [| 3, 90; 1, 0; 0, 90; 4, 0 |] |]

let inFace w (x, y) (x0, y0) =
    x >= x0 && x < x0 + w && y >= y0 && y < y0 + w

let (%%) x y = if x >= 0 then x % y else x % y + y

let (%%%) (x, y) w = x %% w, y %% w

let ( ** ) (x, y) v = x * v, y * v
let (/.) (x, y) v = x / v, y / v

// find the face for the point, and translate p to this face local coordinates
let toFace p state =
    let f = state.Faces |> Array.findIndex (inFace state.Width p)
    let fp = state.Faces[f]
    f, (p -- fp)

// find face on the side
let outFace (f, (x, y)) state =
    if x >= state.Width then state.Cnx[f][0]
    elif y >= state.Width then state.Cnx[f][1]
    elif x < 0 then state.Cnx[f][2]
    elif y < 0 then state.Cnx[f][3]
    else f, 0

let toCube (f, p) state = p ++ state.Faces[f]

let rotRight state p =
    let w2 = state.Width - 1
    let center = w2, w2
    (right ((p ** 2) -- center) ++ center) /. 2

let rotLeft state p =
    let w2 = state.Width - 1
    let center = w2, w2
    (left ((p ** 2) -- center) ++ center) /. 2


let advanceCube state =
    let f, pf = toFace state.Pos state
    let localNext = pf ++ state.Dir
    let nf, dirchange = outFace (f, localNext) state
    let localNewFace = (localNext %%% state.Width)

    let n, nd =
        match dirchange with
        | 0 -> localNewFace, state.Dir
        | 90 -> rotLeft state localNewFace, left state.Dir
        | 180 -> localNewFace |> rotRight state |> rotRight state, state.Dir |> right |> right
        | 270 -> rotRight state localNewFace, right state.Dir
        | _ -> failwith "Not expected"

    toCube (nf, n) state, nd

let moveCube state =
    match state.Moves with
    | [] -> state
    | Left :: rest ->
        let dx, dy = state.Dir

        { state with
            Dir = left state.Dir
            Moves = rest }
    | Right :: rest ->
        let dx, dy = state.Dir

        { state with
            Dir = right state.Dir
            Moves = rest }
    | Advance :: rest ->
        let np, nd = advanceCube state

        match get np state with
        | '.' ->
            { state with
                Pos = np
                Dir = nd
                Moves = rest }
        | '#' -> { state with Moves = rest }
        | _ -> failwith "unexpected"


let rec runCube state =
    match state.Moves with
    | [] -> state
    | _ -> moveCube state |> runCube

let print state =
    for y, l in Seq.indexed state.Map do
        for x, c in Seq.indexed l do
            if (x, y) = state.Pos then
                match state.Dir with
                | 1, 0 -> printf ">"
                | 0, 1 -> printf "v"
                | -1, 0 -> printf "<"
                | 0, -1 -> printf "^"
                | _ -> printf "?"
            else
                printf "%c" c

        printfn ""

    for m in state.Moves |> Seq.truncate 10 do
        match m with
        | Advance -> printf "A"
        | Left -> printf "L"
        | Right -> printf "R"

    printfn ""

let rec moveCuben n state =
    if n = 0 then
        state
    else
        state |> moveCube |> moveCuben (n - 1)


input
|> parse (fun s ->
    { s with
        Faces = inputFaces
        Cnx = inputCnx
        Width = 4 })
|> runCube
|> password

System.IO.File.ReadAllLines("input/day22.txt")
|> parse (fun s ->
    { s with
        Faces = faces
        Cnx = cnx
        Width = 50 })
|> runCube
|> password
