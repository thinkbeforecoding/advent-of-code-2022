let input =
    """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""
        .Split('\n')
    |> Array.toList

// accumulate both total size and saveSize on the fly
let rec ls totalSize saveSize lines =
    match lines with
    | [] -> totalSize, saveSize, []
    | head :: tail ->
        if head = "$ cd .." then
            // we do nothing on cd, just stop aggregating files size
            totalSize, saveSize, tail
        elif head.StartsWith "$ cd" then
            // go done one level, skip the ls that is always just after cd
            // the dirSize is the totalSize of the subfolder
            // s is saved size inside subfolder
            let dirSize, s, tail = ls 0 0 (List.tail tail)
            // if total dir size is small enough add it to savings
            let save = if dirSize <= 100000 then dirSize else 0
            ls (totalSize + dirSize) (saveSize + save + s) tail
        elif head.StartsWith "dir" then
            // we don't use names, so just skip it
            ls totalSize saveSize tail
        else
            // compute file size, and add it to total folder size
            let size = int (head.Split(' ')[0])
            ls (totalSize + size) saveSize tail


"""$ cd /
$ ls
dir a
5 j
5 k
$ cd a
$ ls
3 n
$ cd .."""
    .Split('\n')
|> Array.toList
|> ls 0 0

ls 0 0 input

System.IO.File.ReadAllLines("input/day07.txt") |> Array.toList |> ls 0 0

let (totalSize, _, _) =
    System.IO.File.ReadAllLines("input/day07.txt") |> Array.toList |> ls 0 0

let ununsed = 70000000 - totalSize
let toFree = 30000000 - ununsed

// it would be tempting to use 0 as initial value for delete, but we want the min
// so it's better to start from a high value. Here the answer will be smaller than 30000000
// (the total space we need and don't have), so we can use this value
let rec ls' totalSize delete lines =
    match lines with
    | [] -> totalSize, delete, []
    | head :: tail ->
        if head = "$ cd .." then
            totalSize, delete, tail
        elif head.StartsWith "$ cd" then
            let dirSize, d, tail = ls' 0 30000000 (List.tail tail)

            let newDel =
                if dirSize >= toFree then
                    // this dir is elegible for delete
                    min (min dirSize d) delete
                else
                    // file to delete can be found in sub folders also
                    min delete d

            ls' (totalSize + dirSize) newDel tail
        elif head.StartsWith "dir" then
            ls' totalSize delete tail
        else
            let size = int (head.Split(' ')[0])
            ls' (totalSize + size) delete tail

System.IO.File.ReadAllLines("input/day07.txt")
|> Array.toList
|> ls' 0 30000000
|> fun (_, delete, _) -> delete
