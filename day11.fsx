// Day 11: Monkey in the Middle
let input =
    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""
        .Split('\n')

type Operation =
    | Add of int64
    | Mul of int64
    | Square

type Monkey =
    { Items: int64 list
      InspectedItems: int64
      Behavior: Behavior }

and Behavior =
    { Op: Operation
      Test: int64
      TrueCase: int
      FalseCase: int }

let parseMonkey (lines: string[]) =
    { Items = lines[ 1 ].Substring(18).Split(',') |> Array.map int64 |> Array.toList
      InspectedItems = 0
      Behavior =
        { Op =
            if lines[ 2 ].Contains("+") then
                Add(int (lines[ 2 ].Substring(25)))
            elif lines[ 2 ].EndsWith("old") then
                Square
            else
                Mul(int (lines[ 2 ].Substring(25)))
          Test = int (lines[ 3 ].Substring(21))
          TrueCase = int (lines[ 4 ].Substring(29))
          FalseCase = int (lines[ 5 ].Substring(30)) } }

let parse lines =
    lines
    |> Array.chunkBySize 7
    |> Array.map parseMonkey
    |> Array.indexed
    |> Map.ofArray


let apply item =
    function
    | Add n -> item + n
    | Mul n -> item * n
    | Square -> item * item

// manage is a function to manage stress
// this is /3 for part 1, and % the product of tests in part 2
// returns (destination monkey, worry level)
let behave manage (item: int64) behavior =
    let worry =
        let w = apply item behavior.Op
        manage w

    if (worry % behavior.Test) = 0L then
        behavior.TrueCase, worry
    else
        behavior.FalseCase, worry

// the turn for a single monkey.
// returns the new version of the monkey as well as items thrown
let turn manage monkey =
    let rec step monkey thrown =
        match monkey.Items with
        | [] -> monkey, List.rev thrown
        | item :: tail ->
            let monkey =
                { monkey with
                    Items = tail
                    InspectedItems = monkey.InspectedItems + 1L }

            step monkey (behave manage item monkey.Behavior :: thrown)

    step monkey []


// modify the monkeys map to throw an item to dest
let throw monkeys (dest, worry) =
    Map.change
        dest
        (function
        | Some m -> Some { m with Items = m.Items @ [ worry ] }
        | None -> None)
        monkeys

// for a monkey with given index, compute all items inspected
// and move them to other monkeys
let fullTurn manage monkeys index =
    let monkey, thrown = turn manage (Map.find index monkeys)
    (Map.add index monkey monkeys, thrown) ||> List.fold throw

// do a round
let round manage monkeys _ =
    [ 0 .. Map.count monkeys - 1 ] |> List.fold (fullTurn manage) monkeys

// run n rounds
let run manage n monkeys =
    [ 1..n ] |> List.fold (round manage) monkeys

let score monkeys =
    monkeys
    |> Map.toSeq
    |> Seq.map (fun (_, m) -> m.InspectedItems)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) 1L

System.IO.File.ReadAllLines("input/day11.txt")
|> parse
|> run (fun w -> w / 3L) 20
|> score

// part 2
// the worry is always tested for multiples of some values
// so the computation holds if we use modulo these values.
// as we have multiple monkeys, the max level is the product of their test values
let run2 n monkeys =
    let level = monkeys |> Map.fold (fun acc _ m -> acc * m.Behavior.Test) 1L

    run (fun w -> w % level) n monkeys

System.IO.File.ReadAllLines("input/day11.txt") |> parse |> run2 10000 |> score

input |> parse |> run2 10000 |> score
