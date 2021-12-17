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
        buffer @"/home/pk/Repos/AdventOfCode2021/Day6/exampleInput.txt"

    let lanternFishes = Dictionary<int, int>()

    let addFishInternalTimerKey () =
        [ 0 .. 9 ]
        |> List.iter (fun x -> lanternFishes.Add(x, 0))

    let initialiseFishCountsFromInput () =
        stateTrack
        |> List.iter (fun i -> lanternFishes.[i] <- lanternFishes.[i] + 1)

    let initialiseMockFishCounts () =
        lanternFishes.[3] <- 1
        lanternFishes.[5] <- 2
        
    let initialiseSpecificTests () =
        lanternFishes.[0] <- 2
        lanternFishes.[1] <- 1
        lanternFishes.[2] <- 0
        lanternFishes.[3] <- 0
        lanternFishes.[4] <- 0
        lanternFishes.[5] <- 1
        lanternFishes.[6] <- 1
        lanternFishes.[7] <- 1
        lanternFishes.[8] <- 1
        lanternFishes.[9] <- 2

    addFishInternalTimerKey ()
    initialiseFishCountsFromInput ()
//    initialiseMockFishCounts ()
    
    initialiseSpecificTests ()

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
                            lanternFishes.[9] <- lanternFishes.[0]
                            lanternFishes.[0] <- 0
                        | _ -> failwith "lanternFishes.[0] edge"
                    | 7 ->
                        match lanternFishes.[7] with
                        | 0 -> lanternFishes.[7] <- 0
                        | x when x > 0 ->
                            lanternFishes.[6] <- lanternFishes.[7] + lanternFishes.[9]
                            lanternFishes.[7] <- lanternFishes.[8]
                            lanternFishes.[8] <- lanternFishes.[9]
//                            lanternFishes.[9] <- 0
                        | _ -> failwith "lanternFishes.[7] edge"
//                    | 8 ->
//                        match lanternFishes.[8] with
//                        | 0 -> lanternFishes.[8] <- 0
//                        | _ -> lanternFishes.[8] <- lanternFishes.[8]
//                    | 9 ->
//                        match lanternFishes.[9] with
//                        | 0 -> lanternFishes.[9] <- lanternFishes.[0]
//                        | x when x > 0 ->
//                            lanternFishes.[8] <- lanternFishes.[9]
//                            lanternFishes.[6] <- lanternFishes.[9]
//                            lanternFishes.[9] <- 0
//                        | _ -> failwith "lanternFishes.[9] edge"
                    | x when x > 0 && x < 7 ->
                        match lanternFishes.[fishAge.Key] with
                        | 0 -> lanternFishes.[fishAge.Key] <- 0
                        | x when x > 0 ->
                            lanternFishes.[fishAge.Key - 1] <- lanternFishes.[fishAge.Key]
                            lanternFishes.[fishAge.Key] <- 0
                        | _ -> failwith "lanternFishes.[fishAge.Key] edge"
                    | x when x > 7 -> lanternFishes.[x] <- lanternFishes.[x]
                    | _ -> failwith "outer lanternFishes unhandled")

    countFishesOverNDays 17

    lanternFishes.Remove(9) |> ignore

    lanternFishes.Values |> Seq.iter (fun x -> printfn $"{x}")
    
    lanternFishes.Values |> Seq.sum


lanternFishExponentialGrowth () 