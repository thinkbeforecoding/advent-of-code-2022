// Day 21: Monkey Math

#r "nuget: FParsec"
open FParsec

let input =
    """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""

type Monkey =
    | Int of int64
    | Computation of string * char * string

let pName = identifier (IdentifierOptions())

let pNameDef = pName .>> pchar ':' .>> spaces

let pValue = pint64 |>> Int

let pCompute =
    pipe3 (pName .>> spaces) (anyChar .>> spaces) pName (fun m1 op m2 -> Computation(m1, op, m2))

let pMonkey = pNameDef .>>. (pValue <|> pCompute) .>> spaces

let parseLine input =
    match run pMonkey input with
    | Success(r, _, _) -> r
    | Failure(m, _, _) -> failwith m

let parse input =
    input |> Array.map parseLine |> Map.ofArray


let rec eval name monkeys =
    match Map.find name monkeys with
    | Int n -> n
    | Computation(m1, op, m2) ->
        let v1 = eval m1 monkeys
        let v2 = eval m2 monkeys

        match op with
        | '+' -> v1 + v2
        | '-' -> v1 - v2
        | '*' -> v1 * v2
        | '/' -> v1 / v2
        | _ -> failwith "Unknown op"

input.Split('\n') |> parse |> eval "root"

System.IO.File.ReadAllLines("input/day21.txt") |> parse |> eval "root"



let rec contains root name monkeys =
    if root = name then
        true
    else
        match Map.find root monkeys with
        | Int _ -> false
        | Computation(m1, _, m2) -> contains m1 name monkeys || contains m2 name monkeys


let findExpr name monkeys =
    match Map.find name monkeys with
    | Computation(left, op, right) ->
        if contains left "humn" monkeys then
            left, (op, false), eval right monkeys
        else
            right, (op, true), eval left monkeys
    | _ -> failwith "root monkey error"

let rec resolve root value monkeys =
    if root = "humn" then
        value
    else
        let exprName, op, v = findExpr root monkeys

        let nextValue =
            if root = "root" then
                v
            else
                match op with
                | '+', _ ->
                    // was value = v+x => x = value-v
                    value - v
                | '-', false ->
                    // was value = x-v => x = value+v
                    value + v
                | '-', true ->
                    // was value = v-x => x = v-value
                    v - value
                | '*', _ -> value / v
                | '/', false ->
                    // was value = x/v => x = value * v
                    value * v
                | '/', true ->
                    // was value = v/x => x = v/value
                    v / value
                | _ -> failwith "unknown op"

        resolve exprName nextValue monkeys





input.Split('\n') |> parse |> resolve "root" 0L


System.IO.File.ReadAllLines("input/day21.txt") |> parse |> resolve "root" 0L
