open System

// let input =
//     """2,2,2
// 1,2,2
// 3,2,2
// 2,1,2
// 2,3,2
// 2,2,1
// 2,2,3
// 2,2,4
// 2,2,6
// 1,2,5
// 3,2,5
// 2,1,5
// 2,3,5"""

let parse (input: string) =
    input.Split('\n')
    |> Array.map (fun line ->
        let p = line.Split(',')
        int p[0], int p[1], int p[2])

let (++) (x1, y1, z1) (x2, y2, z2) = x1 + x2, y1 + y2, z1 + z2

let dim points =
    (points
     |> Array.fold (fun (xa, ya, za) (x, y, z) -> max xa x, max ya y, max za z) (0, 0, 0))
    ++ (3, 3, 3)

let points =
    IO.File.ReadAllText("input/day18.txt")
    // input
    |> parse

let dx, dy, dz = dim points

let space = Array3D.zeroCreate<int> dx dy dz

for (x, y, z) in points do
    space[x + 1, y + 1, z + 1] <- 1


let inside (x, y, z) (space: _[,,]) =
    x >= 0
    && x < space.GetLength(0)
    && y >= 0
    && y < space.GetLength(1)
    && z >= 0
    && z < space.GetLength(2)

let get (x, y, z) (space: _[,,]) = space[x, y, z]

let set (x, y, z) v (space: _[,,]) = space[x, y, z] <- v

let rec count d pos c inout (space: _[,,]) =
    if inside pos space then
        let p = get pos space <> 0

        if p = inout then
            count d (pos ++ d) c inout space
        else
            count d (pos ++ d) (c + 1) (not inout) space
    else if not inout then
        c
    else
        (c + 1)



[ for y in 0 .. dy - 1 do
      for x in 0 .. dx - 1 do
          count (0, 0, 1) (x, y, 0) 0 false space
  for y in 0 .. dy - 1 do
      for z in 0 .. dz - 1 do
          count (1, 0, 0) (0, y, z) 0 false space
  for x in 0 .. dx - 1 do
      for z in 0 .. dz - 1 do
          count (0, 1, 0) (x, 0, z) 0 false space ]
|> List.sum

let add pos space toVisit =
    if inside pos space && get pos space = 0 then
        pos :: toVisit
    else
        toVisit

let rec fill (space: _[,,]) toVisit =
    match toVisit with
    | [] -> space
    | pos :: rest ->
        match get pos space with
        | 0 ->
            set pos -1 space

            rest
            |> add (pos ++ (-1, 0, 0)) space
            |> add (pos ++ (1, 0, 0)) space
            |> add (pos ++ (0, -1, 0)) space
            |> add (pos ++ (0, 1, 0)) space
            |> add (pos ++ (0, 0, -1)) space
            |> add (pos ++ (0, 0, 1)) space
            |> fill space
        | _ -> fill space rest

let rec count2 d pos c inout (space: _[,,]) =
    if inside pos space then
        let p = get pos space >= 0

        if p = inout then
            count2 d (pos ++ d) c inout space
        else
            count2 d (pos ++ d) (c + 1) (not inout) space
    else if not inout then
        c
    else
        (c + 1)


let space2 =
    let s = Array3D.zeroCreate<int> dx dy dz

    for (x, y, z) in points do
        s[x + 1, y + 1, z + 1] <- 1

    fill
        s
        [ 0, 0, 0
          dx - 1, 0, 0
          0, dy - 1, 0
          dx - 1, dy - 1, 0
          0, 0, dz - 1
          dx - 1, 0, dz - 1
          0, dy - 1, dz - 1
          dx - 1, dy - 1, dz - 1 ]

[ for y in 0 .. dy - 1 do
      for x in 0 .. dx - 1 do
          count2 (0, 0, 1) (x, y, 0) 0 false space2
  for y in 0 .. dy - 1 do
      for z in 0 .. dz - 1 do
          count2 (1, 0, 0) (0, y, z) 0 false space2
  for x in 0 .. dx - 1 do
      for z in 0 .. dz - 1 do
          count2 (0, 1, 0) (x, 0, z) 0 false space2 ]
|> List.sum
