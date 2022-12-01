open System.IO

File.ReadAllLines("input/day01.txt")
|> Array.fold (fun (best, current) line -> 
    if line = "" then
        max best current, 0
    else
        best, current + int line
) (0,0)
|> fst

File.ReadAllLines("input/day01.txt")
|> Array.fold (fun (elves, current) line -> 
    if line = "" then
        current :: elves, 0
    else
        elves, current + int line
) ([],0)
|> fst
|> List.sortDescending
|> List.take 3
|> List.sum