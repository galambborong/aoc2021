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
        
    let initialiseMockFishCounts () =
        lanternFishes.[3] <- 1
        lanternFishes.[5] <- 2

    addFishInternalTimerKey ()
    initialiseFishCountsFromInput ()
//    initialiseMockFishCounts ()

    let countFishesOverNDays n =
        [ 0 .. n ]
        |> List.iter
            (fun _ ->
                printfn $"\n"
                for fishAge in lanternFishes do
                    printfn $"current Key {fishAge.Key}, current value {fishAge.Value}"

                    match fishAge.Key with
                    | 0 ->
                        match lanternFishes.[0] with
                        | 0 -> lanternFishes.[0] <- 0
                        | x when x > 0 ->
                                          lanternFishes.[8] <- lanternFishes.[0] 
                                          lanternFishes.[6] <- lanternFishes.[0]
                                          lanternFishes.[0] <- 0
                        | _ -> failwith "lanternFishes.[0] edge"
                    | _ ->
                        match lanternFishes.[fishAge.Key] with
                        | 0 -> lanternFishes.[fishAge.Key] <- 0
                        | x when x > 0 ->
                                          lanternFishes.[fishAge.Key - 1] <- lanternFishes.[fishAge.Key]
                                          lanternFishes.[fishAge.Key] <- 0
                        | _ -> failwith "lanternFishes.[fishAge.Key] edge")

    countFishesOverNDays 2

    lanternFishes.Values |> Seq.sum



// lanternFishExponentialGrowth()
