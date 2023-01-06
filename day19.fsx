open System.IO
open System.Text.RegularExpressions

let input =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
        .Split('\n')

let parse (input: string[]) =
    input
    |> Array.map (fun l ->
        let m =
            Regex.Match(
                l,
                @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
            )

        let id = int m.Groups[1].Value
        let oreRobot = int m.Groups[2].Value, 0, 0, 0
        let clayRobot = int m.Groups[3].Value, 0, 0, 0
        let obsidianRobot = int m.Groups[4].Value, int m.Groups[5].Value, 0, 0
        let geodeRobot = int m.Groups[6].Value, 0, int m.Groups[7].Value, 0

        id, oreRobot, clayRobot, obsidianRobot, geodeRobot)



let (>=!) (a1, b1, c1, d1) (a2, b2, c2, d2) =
    a1 >= a2 && b1 >= b2 && c1 >= c2 && d1 >= d2

let (++) (a1, b1, c1, d1) (a2, b2, c2, d2) = a1 + a2, b1 + b2, c1 + c2, d1 + d2
let (--) (a1, b1, c1, d1) (a2, b2, c2, d2) = a1 - a2, b1 - b2, c1 - c2, d1 - d2

let addStack cond rounds collecting resource stack =
    if cond then
        (rounds, collecting, resource) :: stack
    else
        stack

let _ore (o, _, _, _) = o
let _clay (_, c, _, _) = c
let _obs (_, _, o, _) = o

// Day 19: Not Enough Minerals

let pmax p = List.map p >> List.max

let run totalRounds (id, ore, clay, obs, geode) =
    let maxOre = pmax _ore [ ore; clay; obs; geode ]
    let maxClay = pmax _clay [ ore; clay; obs; geode ]
    let maxObs = pmax _obs [ ore; clay; obs; geode ]

    printfn $"[{id}] Run"

    let rec loop maximum stack =
        match stack with
        | [] ->
            printfn $"[{id}]Result {maximum}"
            maximum
        | (rounds, collecting, resource) :: rest ->
            if rounds = 0 then
                let (_, _, o, g) = resource
                // if g > 0 then
                //     printfn $"End {resource}/ max {maximum}"
                if g > maximum then
                    printfn $"[{id}]End {resource} / {collecting}/ max {g}"
                    loop g rest
                else
                    loop maximum rest
            else
                let (_, _, _, g) = resource
                let (_, _, _, cg) = collecting

                if
                    g + cg * rounds + (rounds + 1) * rounds / 2
                    <= maximum (*|| (cg=0 && o+rounds*(max co 1) < no)*)
                then
                    // shortcut, there is now way it can be better
                    loop maximum rest
                elif resource >=! geode then
                    loop
                        maximum
                        ((rounds - 1, (collecting ++ (0, 0, 0, 1)), (resource -- geode ++ collecting))
                         :: rest)
                else
                    rest
                    |> addStack true (rounds - 1) collecting (resource ++ collecting)
                    |> addStack
                        (resource >=! ore && _ore collecting < maxOre)
                        (rounds - 1)
                        (collecting ++ (1, 0, 0, 0))
                        (resource -- ore ++ collecting)
                    |> addStack
                        (resource >=! clay && _clay collecting < maxClay)
                        (rounds - 1)
                        (collecting ++ (0, 1, 0, 0))
                        (resource -- clay ++ collecting)
                    |> addStack
                        (resource >=! obs && _obs collecting < maxObs)
                        (rounds - 1)
                        (collecting ++ (0, 0, 1, 0))
                        (resource -- obs ++ collecting)
                    |> loop maximum


    loop 0 [ totalRounds, (1, 0, 0, 0), (0, 0, 0, 0) ]




let blueprints =
    // input
    File.ReadAllLines("input/day19.txt") |> parse

open System.Linq

blueprints
|> fun a -> a.AsParallel().Select(fun ((id, _, _, _, _) as c) -> id * run 24 c)
|> Seq.sum

blueprints
|> Array.take 3
|> fun a -> a.AsParallel().Select(fun c -> run 32 c)
|> Seq.fold (*) 1
