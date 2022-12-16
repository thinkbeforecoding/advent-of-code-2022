// Day 16: Proboscidea Volcanium
#r "nuget: FParsec"
open FParsec

let input =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

let proom = anyString 2

let pval =
    skipString "Valve " >>. proom
    .>>. tuple2
             (skipManySatisfy (isDigit >> not) >>. pint32)
             (skipManySatisfy (isAsciiUpper >> not) >>. sepBy proom (skipChar ',' .>> spaces))

let pfile = sepBy pval newline

let parse input =
    match run pfile input with
    | Success(v, _, _) -> v |> Map.ofList
    | Failure(e, _, _) -> failwith e


let mutable maxReleased = 0


let rec run time roomName prevRoom totalFlow released remainingFlow (valves: Map<_, _>) =
    // printfn $"[{time}] {roomName} {totalFlow} {released} {remainingFlow}"
    if released + (totalFlow + remainingFlow) * time < maxReleased then
        // even with every valve open from now, we cannot do better
        ()
    elif remainingFlow = 0 then
        // all valves are open.. we can compute directly the total
        // whatever we do next

        maxReleased <- max maxReleased (released + time * totalFlow)
    elif time = 0 then
        // we ran out of time.
        // check if we did better
        maxReleased <- max maxReleased released
    else
        let flow, nextRooms = valves[roomName]

        if flow > 0 then
            // there is a valve that has not been open yet
            // explore the possibility of opening it
            let valves = Map.add roomName (0, nextRooms) valves
            // flow is increased, and remaining flow is decreased
            // we stay in the same room and will move at next minute
            run (time - 1) roomName prevRoom (totalFlow + flow) (released + totalFlow) (remainingFlow - flow) valves

        do
            let toExplore =
                match nextRooms with
                | [ _ ] -> // there is only one way, take it even if we come from there
                    nextRooms :> _ seq
                | _ ->
                    // there are multiple ways, don't go back, it will never be a better solution
                    nextRooms |> Seq.filter (fun r -> r <> prevRoom)

            for nxt in toExplore do
                // explore all other rooms
                run (time - 1) nxt roomName totalFlow (totalFlow + released) remainingFlow valves


maxReleased <- 0
// input
System.IO.File.ReadAllText "input/day16.txt"
|> parse
|> fun valves ->
    let remainingFlow = valves |> Map.values |> Seq.sumBy fst
    run 30 "AA" "" 0 0 remainingFlow valves

printfn $"MaxReleased: {maxReleased}"



// now we need to trac position and elephant position
let rec run2 time roomName prevRoom elRoom elPrev totalFlow released remainingFlow (valves: Map<_, _>) =
    if released + (totalFlow + remainingFlow) * time < maxReleased then
        // no way we can do better from now on
        ()
    elif remainingFlow = 0 then
        // we cannot increase flow, we know what the result will be
        maxReleased <- max maxReleased (released + time * totalFlow)
    elif time = 0 then
        // time it out, is it a better result ?
        maxReleased <- max maxReleased released
    else
        // get current room for player and elephant
        match valves[roomName], valves[elRoom] with
        | (flow, nextRooms), (elFlow, elNextRooms) ->

            if flow > 0 && elFlow > 0 && roomName <> elRoom then
                // player and elephant are in a room with a closed valve (and this is not the same one)
                // open both valves
                let valves =
                    valves |> Map.add roomName (0, nextRooms) |> Map.add elRoom (0, elNextRooms)

                run2
                    (time - 1)
                    roomName
                    prevRoom
                    elRoom
                    elPrev
                    (totalFlow + flow + elFlow)
                    (released + totalFlow)
                    (remainingFlow - flow - elFlow)
                    valves

            if flow > 0 then
                // player is in a room with a valve, open it
                // pass also here when both player an elephant are in the same room with a valve
                // but only the player opens it
                let valves = Map.add roomName (0, nextRooms) valves
                // and let the elephant move
                let elToExplore =
                    match elNextRooms with
                    | [ r ] -> elNextRooms :> _ seq
                    | _ -> elNextRooms |> Seq.filter (fun r -> r <> elPrev)

                for elNxt in elToExplore do
                    run2
                        (time - 1)
                        roomName
                        prevRoom
                        elNxt
                        elRoom
                        (totalFlow + flow)
                        (released + totalFlow)
                        (remainingFlow - flow)
                        valves

            if elFlow > 0 then
                // elephant is in a room with a valve, open it
                // pass also here when both player an elephant are in the same room with a valve
                // but only the elephant opens it
                let valves = Map.add elRoom (0, elNextRooms) valves
                // and the player moves
                let toExplore =
                    match nextRooms with
                    | [ r ] -> nextRooms :> _ seq
                    | _ -> nextRooms |> Seq.filter (fun r -> r <> prevRoom)

                for nxt in toExplore do
                    run2
                        (time - 1)
                        nxt
                        roomName
                        elRoom
                        elPrev
                        (totalFlow + elFlow)
                        (released + totalFlow)
                        (remainingFlow - elFlow)
                        valves

            do
                // in all cases we also explore when both the player and the elepant move without opening the valve
                let toExplore =
                    match nextRooms with
                    | [ r ] -> nextRooms :> _ seq
                    | _ -> nextRooms |> Seq.filter (fun r -> r <> prevRoom)

                let elToExplore =
                    match elNextRooms with
                    | [ r ] -> elNextRooms :> _ seq
                    | _ -> elNextRooms |> Seq.filter (fun r -> r <> elPrev)

                for nxt in toExplore do
                    for elNxt in elToExplore do
                        run2 (time - 1) nxt roomName elNxt elRoom totalFlow (totalFlow + released) remainingFlow valves

#time
maxReleased <- 0
// input
System.IO.File.ReadAllText "input/day16.txt"
|> parse
|> fun valves ->
    let remainingFlow = valves |> Map.values |> Seq.sumBy fst
    run2 26 "AA" "" "AA" "" 0 0 remainingFlow valves

printfn $"MaxReleased: {maxReleased}"
