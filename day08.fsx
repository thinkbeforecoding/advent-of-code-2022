let input =
    """30373
25512
65332
33549
35390"""

type Forest =
    { Trees: string
      Width: int
      Height: int }


let forest (input: string) =
    let input = input.ReplaceLineEndings("n")
    let w = input.IndexOf('n')
    let h = input.Length / w
    { Trees = input; Width = w; Height = h }


let getTree forest (x, y) =
    forest.Trees[x + y * (forest.Width + 1)]


let add (x, y) (dx, dy) = x + dx, y + dy

let inForest f (x, y) =
    x >= 0 && x < f.Width && y >= 0 && y < f.Height

let rec visible forest move pos tallest trees =
    let tree = getTree forest pos
    let trees = if tree > tallest then Set.add pos trees else trees
    let pos = add pos move

    if inForest forest pos then
        visible forest move pos (max tallest tree) trees
    else
        trees


let allTrees f =
    [ for y in 0 .. f.Height - 1 do
          visible f (1, 0) (0, y) (char 0) Set.empty
          visible f (-1, 0) (f.Width - 1, y) (char 0) Set.empty
      for x in 0 .. f.Width - 1 do
          visible f (0, 1) (x, 0) (char 0) Set.empty
          visible f (0, -1) (x, f.Height - 1) (char 0) Set.empty ]
    |> Set.unionMany
    |> Set.count


System.IO.File.ReadAllText("input/day08.txt") |> forest |> allTrees

let rec distance f dir pos tallest dist =
    let pos = add pos dir

    if not (inForest f pos) then
        dist
    else
        let tree = getTree f pos

        if tree < tallest then
            distance f dir pos tallest (dist + 1)
        else
            dist + 1

let startDistance f dir pos = distance f dir pos (getTree f pos) 0

let f = forest input

let scenic f pos =
    startDistance f (1, 0) pos
    * startDistance f (-1, 0) pos
    * startDistance f (0, 1) pos
    * startDistance f (0, -1) pos


let bestPos f =
    seq {
        for x in 0 .. f.Width - 1 do
            for y in 0 .. f.Height - 1 do
                (x, y)
    }
    |> Seq.map (scenic f)
    |> Seq.max

System.IO.File.ReadAllText("input/day08.txt") |> forest |> bestPos
