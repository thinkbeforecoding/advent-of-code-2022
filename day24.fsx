let input =
    """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""

type Vec = int * int

type Map =
    { Map: string
      Size: Vec
      Blizzards: (Vec * Vec) list
      Pos: Vec
      Output: Vec }

let up = (0, -1)
let right = (1, 0)
let down = (0, 1)
let left = (-1, 0)

let parse (input: string) =
    let m = input.ReplaceLineEndings("")
    let w = input.IndexOfAny [| '\n'; '\r' |]
    let h = m.Length / w

    let b =
        [ for y in 0 .. h - 1 do
              for x in 0 .. w - 1 do
                  match m[x + y * w] with
                  | '^' -> (x, y), up
                  | '>' -> (x, y), right
                  | 'v' -> (x, y), down
                  | '<' -> (x, y), left
                  | _ -> () ]

    let pos = m.IndexOf('.'), 0
    let iout = m.LastIndexOf('.')
    let out = iout % w, iout / w

    { Map = m
      Size = w, h
      Blizzards = b
      Pos = pos
      Output = out }




let (++) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let (--) (x1, y1) (x2, y2) = x1 - x2, y1 - y2
let ( ** ) (x, y) n = x * n, y * n

let (%%) x y =
    let m = x % y
    if m >= 0 then m else m + y

let x = fst
let y = snd

let norm (x, y) = abs x + abs y
let dist a b = norm (a -- b)

let get map (x, y) = map.Map[x + y * fst map.Size]

let moveBlizzard n (w, h) (pos, dir) =
    let x, y = pos ++ dir ** n
    ((x - 1) %% (w - 2)) + 1, ((y - 1) %% (h - 2)) + 1

let isFree (x, y) map =
    let w, h = map.Size
    x >= 0 && x < w && y >= 0 && y < h && map.Map[x + y * w] <> '#'

let canMove pos dir map bliz =
    let p' = pos ++ dir
    isFree p' map && not (Set.contains p' bliz)

let move n pos dir map bliz stack =
    if canMove pos dir map bliz then
        let p' = pos ++ dir
        let h = n + dist p' map.Output
        Set.add (h, n, pos ++ dir) stack
    else
        stack

let stay n pos map bliz stack =
    if Set.contains pos bliz then
        stack
    else
        let h = n + dist pos map.Output
        Set.add (h, n, pos) stack

type Action =
    | Stop of int
    | Continue of int * (int * int * Vec) Set

let step map action =
    match action with
    | Stop n -> action
    | Continue(mindist, stack) when Set.isEmpty stack -> Stop mindist
    | Continue(mindist, stack) ->
        let (_, n, pos) as e = Set.minElement stack
        let rest = Set.remove e stack

        if pos = map.Output then
            Continue(min mindist n, rest)
        elif n + dist pos map.Output >= mindist then
            Continue(mindist, rest)
        else
            let n' = n + 1
            let bliz = map.Blizzards |> List.map (moveBlizzard n' map.Size) |> set

            let newStack =
                rest
                |> stay n' pos map bliz
                |> move n' pos left map bliz
                |> move n' pos up map bliz
                |> move n' pos right map bliz
                |> move n' pos down map bliz

            Continue(mindist, newStack)

let rec run map action =
    match step map action with
    | Stop n -> n
    | Continue _ as a -> run map a

let print map action =
    match action with
    | Stop n -> printfn "Result: %d" n
    | Continue(mindist, stack) ->
        match stack with
        | [] -> ()
        | (n, pos) :: _ ->
            let bliz =
                map.Blizzards
                |> List.map (fun (b, d) -> moveBlizzard n map.Size (b, d), d)
                |> List.groupBy fst
                |> Map.ofList

            for y in 0 .. snd map.Size - 1 do
                for x in 0 .. fst map.Size - 1 do
                    match Map.tryFind (x, y) bliz with
                    | Some [ _, (0, -1) ] -> printf "^"
                    | Some [ _, (1, 0) ] -> printf ">"
                    | Some [ _, (0, 1) ] -> printf "v"
                    | Some [ _, (-1, 0) ] -> printf "<"
                    | Some bs -> printf $"{List.length bs}"
                    | None ->
                        if (x, y) = pos then printf "E"
                        elif get map (x, y) = '#' then printf "#"
                        else printf " "

                printfn ""




let startStack map =
    Continue(System.Int32.MaxValue, [ 0, map.Pos ])


let map = input |> parse
let stack = startStack map
let stack = step map stack
print map stack

print map (Continue(0, [ 18, map.Pos ]))
