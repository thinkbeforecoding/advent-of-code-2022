
let parse (l: string) = 
    let half = l.Length/2
    l.Substring(0,half), l.Substring(half)

let findCommonItem (x:string,y: string) =
    [ for cx in x do
      for cy in y do
        if cx = cy then
            cx]
    |> List.distinct

let score (c: char) =
    if c >= 'a' && c <= 'z' then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27


System.IO.File.ReadAllLines("input/day03.txt")
|> Seq.collect(parse>>findCommonItem)
|> Seq.sumBy score

let findBadge [|x : string;y;z|] =
    [ for cx in x do
      for cy in y do
      for cz in z do
        if cx = cy && cx = cz then
            cx
    ]
    |> List.distinct


System.IO.File.ReadAllLines("input/day03.txt")
|> Seq.chunkBySize 3
|> Seq.collect findBadge
|> Seq.sumBy score