let parse (l: string) =
    let half = l.Length / 2
    l.Substring(0, half), l.Substring(half)

let findCommonItem (x: string, y: string) = x |> Seq.find (fun c -> y.Contains(c))

let score (c: char) =
    if c >= 'a' && c <= 'z' then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27


System.IO.File.ReadAllLines("input/day03.txt")
|> Seq.sumBy (parse >> findCommonItem >> score)

let findBadge [| x: string; y: string; z: string |] =
    x |> Seq.find (fun c -> y.Contains(c) && z.Contains(c))

System.IO.File.ReadAllLines("input/day03.txt")
|> Seq.chunkBySize 3
|> Seq.sumBy (findBadge >> score)
