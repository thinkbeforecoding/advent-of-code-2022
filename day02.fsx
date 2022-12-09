open System.IO

[<Struct>]
type Hand = Hand of int

let hand x =
    let r = x % 3
    if r >= 0 then Hand r else Hand(r + 3)

let rock = Hand 0
let paper = Hand 1
let cisor = Hand 2

let (+.) (Hand h) x = hand (h + x)

let better hand = hand +. 1
let worse hand = hand +. -1

module Game =
    let parse c =
        match c with
        | 'A'
        | 'X' -> rock
        | 'B'
        | 'Y' -> paper
        | 'C'
        | 'Z' -> cisor

    let value (Hand h) = h + 1

    let winnerScore other self =
        if other = self then 3
        elif self = better other then 6
        else 0

    let score (other, self) = winnerScore other self + value self

let play other =
    function
    | 'X' -> worse other
    | 'Y' -> other
    | 'Z' -> better other

let parse (line: string) = Game.parse line[0], Game.parse line[2]

File.ReadAllLines("input/day02.txt") |> Seq.sumBy (parse >> Game.score)

File.ReadAllLines("input/day02.txt")
|> Seq.sumBy (fun line ->
    let other = Game.parse line[0]
    let self = play other line[2]
    Game.score (other, self))
