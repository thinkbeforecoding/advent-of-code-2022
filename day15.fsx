// Day 15: Beacon Exclusion Zone

#r "nuget: FParsec"
open FParsec

let input =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

// parsing done using FParsec
let pvector = skipString "x=" >>. pint32 .>> skipString ", y=" .>>. pint32

let psensor =
    skipString "Sensor at " >>. pvector .>> skipString ": closest beacon is at "
    .>>. pvector

let pFile = sepBy psensor newline

let parse input =
    match run pFile input with
    | Success(v, _, _) -> v
    | Failure(e, _, _) -> failwith e

// this is the manhattan distance
let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

// find all points in range of a sensor
let inRange line (sensor, beacon) =
    let d = dist sensor beacon // max dist for sensor
    let dy = abs (line - snd sensor) // we know the y distance from line to sensor
    let dx = d - dy // so max x range is dx
    // returns all points in range
    [ fst sensor - dx .. fst sensor + dx ]

// we need to remove beacon that are on the line, so find them first
let beaconsOnLine line ls =
    ls |> List.choose (fun (_, (x, y)) -> if y = line then Some x else None)

let part1 line input =
    let sensors = input |> parse

    sensors
    |> List.collect (inRange line) // take all position that are in range of a sensor
    |> List.distinct // remove duplicates
    |> List.except (beaconsOnLine line sensors) // remove actual beacons
    |> List.length // count positions

System.IO.File.ReadAllText("input/day15.txt") |> part1 2000000

// part2
// a simple solition would be to check for each point in the area
// if all sensors are out of range. It works for the sample, but is
// waayy to long with the actual input (it is in O(4000000² * number of sensors) !)
// a better method is to do it line by line (now in 0(4000000 * number of sensors))
// On a line, we can compute the ranges as done before, but just keep start and end.
// The union of the range should cover the full area with, except on the line with
// the solution.
// as we expect the union to cover the area or have a single hole, we can process
// ranges from left to write, and advance an x coordinate, and if a range starts
// after this x, we have found the hole!

// intersection used to restricts range to area
let intersection (s1, e1) (s2, e2) = max s1 s2, min e1 e2

// check if range is empty
let isEmpty (s, e) = s >= e

// finds the hole, if not found, return value will be the end of the area
let rec findHole x ranges =
    match ranges with
    | [] -> x // ranges exhausted, indicates where we are
    | (s, e) :: tail ->
        if s > x then
            x // range starts after, this is the hole
        else
            findHole (max x e) tail // we can merge the range and continue


let findOnLine maxRange line sensors =
    let ranges =
        sensors
        |> List.choose (fun (sensor, beacon) ->
            // compute the range as in part 1,
            // and returns it if it has an intersection with the area
            let d = dist sensor beacon
            let dy = abs (line - snd sensor)
            let dx = d - dy
            let r = intersection (fst sensor - dx, fst sensor + dx + 1) maxRange

            if isEmpty r then None else Some r)
        |> List.sort // sort to process left to right

    let e = findHole 0 ranges // find hole if any

    if e >= snd maxRange then
        // we reached the end of the area no hole
        None
    else
        // we found the hole, compute the result
        Some(int64 e * 4000000L + int64 line)

let part2 high input =
    let measures = input |> parse

    // finds the line with the hole
    [| 1..high |]
    |> Array.Parallel.choose (fun line -> findOnLine (0, high + 1) line measures)
    |> Array.head

System.IO.File.ReadAllText("input/day15.txt") |> part2 4000000
