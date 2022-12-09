open System

let rec allDiff (span: _ ReadOnlySpan) x =
    if x < span.Length - 1 then
        if span.Slice(x + 1).Contains(span[x]) then
            false
        else
            allDiff span (x + 1)
    else
        true

let rec find i l (txt: string) =
    let chars = txt.AsSpan().Slice(i, l)
    if allDiff chars 0 then i + l else find (i + 1) l txt

System.IO.File.ReadAllText("input/day06.txt") |> find 0 4
System.IO.File.ReadAllText("input/day06.txt") |> find 0 14
