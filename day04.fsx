
let mapPair f (x,y) = f x, f y

let parsePair c (s: string) =
    let p = s.Split(c: char)
    p[0],p[1]

let parse line =
    line
    |> parsePair ','
    |> mapPair (parsePair '-' >> mapPair int)

let contains ((s1,e1), (s2,e2)) =
    s1 >= s2 && e1 <= e2

let anyContains (x,y) = contains (x,y) || contains (y,x)

let overlap ((s1,e1), (s2,e2)) =
    not (e1 < s2 || e2 < s1)

"""2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8""".Split('\n')
|> Seq.map parse
|> Seq.filter anyContains
|> Seq.length

System.IO.File.ReadAllLines("input/day04.txt")
|> Seq.map parse
|> Seq.filter anyContains
|> Seq.length

System.IO.File.ReadAllLines("input/day04.txt")
|> Seq.map parse
|> Seq.filter overlap
|> Seq.length

