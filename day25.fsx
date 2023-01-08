// Day 25: Full of Hot Air

let input =
    """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122""".Split('\n')

let parseChar = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '-' -> -1
    | '=' -> -2
    | _ -> failwith "Invalid char"

let parseNumber (s: string) =
    [ for c in s -> parseChar c ] 

let toDecimal n=
    let rec loop v l =
        match l with
        | [] -> v
        | h :: t -> loop (v*5+h) t

    loop 0 n

let zero = [0]

let len num = List.length num

let rec pad n num = 
    if n > List.length num then
        pad n (0 :: num)
    else
        num

let norm x =
    let rec loop x rem =
        if x > 2 then
            loop (x-5) (rem+1)
        elif x < -2 then
            loop (x+5) (rem-1) 
        else
            x,rem
    loop x 0

let normalize num =
    let rem, result =
        (num, (0,[]))
        ||> List.foldBack (fun n (r,acc) ->
            let v, rem = norm (n+r)  
            (rem, v :: acc)

        )
    if rem = 0 then
        result
    else
        rem :: result


let add x y =
    let size = max (len x) (len y)
    let x = pad size x
    let y = pad size y
    List.map2 (+) x y
    |> normalize

let print num =
    for n in num do
        match n with
        | 0 | 1 | 2 -> printf "%d" n
        | -1 -> printf "-"
        | -2 -> printf "="
        | _ -> printf "?"
    printfn ""


input
|> Array.map parseNumber
|> Seq.fold add zero
|> print
// |> toDecimal

System.IO.File.ReadAllLines("input/day25.txt")
|> Array.map parseNumber
|> Seq.fold add zero
|> print