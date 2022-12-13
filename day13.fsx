// Day 13: Distress Signal

// for this one, we use FParsec to parse the input
#r "nuget: FParsec"
open FParsec

let input =
    """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

// We will put data in this recusive structure
type Item =
    | Int of int
    | List of Item list

// pitem is the parser for an item. It is recursive,
// so we use this function, item can be used for the recursion,
// pitem will be the final parser.
let item, pitem = createParserForwardedToRef ()

// this is the parser for a list, it uses the 'item' parser, separated by ','
let plist = sepBy item (pchar ',') |> between (pchar '[') (pchar ']') |>> List

// this is the parse for an int
let pint = pint32 |>> Int

// this is the full pitem parser, an int or a list
pitem.Value <- pint <|> plist

// this is the parse for the file...
// we process them by pairs, the first is ended by a newline
// while the second is ended by 0 or plus lines (because, at the end, there is no newline)
let pfile = tuple2 (pitem.Value .>> newline) (pitem.Value .>> many newline) |> many

let parse input =
    match run pfile input with
    | Success(v, _, _) -> v
    | Failure(e, _, _) -> failwithf "%s" e

// this is our comparison function
// it returns -1 when left < right
// returns 0 when left = right
// returns 1 when left > right
let rec comp (left, right) =
    match left, right with
    | Int l, Int r -> compare l r
    | List [], List [] -> 0 // same
    | List [], List _ -> -1 // left ends before
    | List _, List [] -> 1 // right ends before
    | List(l :: ls), List(r :: rs) ->
        match comp (l, r) with
        | 0 -> comp (List ls, List rs) // continue comp
        | cmp -> cmp // end comp with result
    | Int _, List _ -> comp (List [ left ], right) // make list from item
    | List _, Int _ -> comp (left, List [ right ])

let score l =
    l
    |> List.indexed // add indices to the list
    |> List.choose (fun (i, v) -> if v < 0 then Some(i + 1) else None) // keep when left is smaller than right and returl
    |> List.sum

parse input |> List.map comp |> score

(parse input)[1] |> comp

System.IO.File.ReadAllText("input/day13.txt") |> parse |> List.map comp |> score

// part 2
// we define the two special items
let l2 = List [ List [ Int 2 ] ]
let l6 = List [ List [ Int 6 ] ]
// comp is already a comparator that can be used to sort the list

parse input
|> List.collect (fun (l, r) -> [ l; r ])
|> List.append ([ l2; l6 ])
|> List.sortWith (fun l r -> comp (l, r))
|> fun l -> (List.findIndex ((=) l2) l + 1) * (List.findIndex ((=) l6) l + 1)

System.IO.File.ReadAllText("input/day13.txt")
|> parse
|> List.collect (fun (l, r) -> [ l; r ])
|> List.append ([ l2; l6 ])
|> List.sortWith (fun l r -> comp (l, r))
|> fun l -> (List.findIndex ((=) l2) l + 1) * (List.findIndex ((=) l6) l + 1)
