module DaySix

open System
open System.Collections.Generic
open System.IO


type Fishes = Map<int, int>


let buffer file =
    seq { yield! File.ReadLines file }
    |> Seq.head
    |> Seq.toList
    |> List.filter Char.IsNumber
    |> List.map (string >> int)

let state (internalTimer: int) =
    match internalTimer with
    | 0 -> 6
    | _ -> internalTimer - 1

let lanternFishExponentialGrowth () =
    let stateTrack =
        buffer @"c:/dev/aoc2021/Day6/exampleInput.txt"

    let lanternFishes = Dictionary<int, int>()

    let addFishInternalTimerKey () =
        [ 0 .. 8 ]
        |> List.iter (fun x -> lanternFishes.Add(x, 0))

    let initialiseFishCountsFromInput () =
        stateTrack
        |> List.iter (fun i -> lanternFishes.[i] <- lanternFishes.[i] + 1)

    addFishInternalTimerKey ()
    initialiseFishCountsFromInput ()

    let countFishesOverNDays n =
        [ 0 .. n ]
        |> List.iter
            (fun _ ->
                for fishAge in lanternFishes do
                    printfn $"current Key {fishAge.Key}"
                    match fishAge.Key with
                    | 0 ->
                        match lanternFishes.[0] with
                        | 0 -> lanternFishes.[0]
                        | _ -> 
                                lanternFishes.[0] <- lanternFishes.[0] - 1
                                lanternFishes.[8] <- lanternFishes.[8] + 1
                    | key when key > 0 ->
                        let currentKey = fishAge.Key
                        lanternFishes.[currentKey] <- lanternFishes.[currentKey] - 1
                        lanternFishes.[currentKey - 1] <- lanternFishes.[currentKey - 1] + 1
                    | _ -> failwith "edge case?")

    countFishesOverNDays 1 |> ignore

    lanternFishes //.Values //|> Seq.sum



// lanternFishExponentialGrowth()
