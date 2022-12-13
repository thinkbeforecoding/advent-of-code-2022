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
let item, pitem = createParserForwardedToRef()

// this is the parser for a list, it uses the 'item' parser, separated by ','
let plist = 
    between (pchar '[') (pchar ']')  (sepBy item (pchar ',')) |>> List
 
 // this is the parse for an int
let pint = pint32 |>> Int

// this is the full pitem parser, an int or a list
pitem.Value <-  pint <|> plist 

// this is the parse for the file...
// we process them by pairs, the first is ended by a newline
// while the second is ended by 0 or plus lines (because, at the end, there is no newline)
let pfile =
    many (tuple2 (pitem.Value .>> newline) (pitem.Value .>> many newline) )

let parse input =
    match run pfile input with
    | Success(v,_,_) -> v
    | Failure(e,_,_) -> failwithf "%s" e

// this is our comparison function
// it returns -1 when left < right
// returns 0 when left = right
// returns 1 when left > right
let rec comp (left, right) =
    match left, right with
    | Int l, Int r ->
        // both are int, juste compare them
        compare l r
    | List [], List [] ->
        // both lists are empty, they are equal
        0
    | List [], List _ ->
        // left list ended before right list, left is smaller
        -1
    | List (l :: ls), List (r :: rs) ->
        // both lists have elements, compare them
        let cmp = comp (l, r)
        if cmp < 0 then
            // left was smaller, so list is smaller 
            cmp
        elif cmp = 0 then
            // items are equal, continue processing the rest of the list 
            comp (List ls, List rs)
        else
            // left was greater, stop now
            1 
    | List _, List [] ->
        // left list still contains items, while rigth is terminated
        1
    | Int _, List _ ->
        // put the int on the left in a list to continue comparison
        comp (List [ left ], right) 
    | List _, Int _ ->
        // put the int on the right in a list to continue comparison 
        comp (left, List [ right ] )

let score l =
    l
    |> List.indexed // add indices to the list
    |> List.choose (fun (i,v) -> if v < 0 then Some (i+1) else None) // keep when left is smaller than right and returl
    |> List.sum

parse input |> List.map comp |> score
        
(parse input)[1] |> comp

System.IO.File.ReadAllText("input/day13.txt")
|> parse
|> List.map comp
|> score

// part 2
// we define the two special items
let l2 = List [ List [ Int 2] ]
let l6 = List [ List [ Int 6] ]
// comp is already a comparator that can be used to sort the list

parse input 
|> List.collect (fun (l,r) -> [l;r])
|> List.append ( [ l2 ; l6 ]  )
|> List.sortWith (fun l r -> comp(l,r))
|> fun l ->  (List.findIndex ((=) l2) l + 1) * (List.findIndex ((=) l6) l + 1)

System.IO.File.ReadAllText("input/day13.txt")
|> parse
|> List.collect (fun (l,r) -> [l;r])
|> List.append ( [ l2 ; l6 ]  )
|> List.sortWith (fun l r -> comp(l,r))
|> fun l ->  (List.findIndex ((=) l2) l + 1) * (List.findIndex ((=) l6) l + 1)

