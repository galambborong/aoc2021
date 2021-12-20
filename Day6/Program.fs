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
        buffer @"./Day6/actualInput.txt"

    let lanternFishes = Dictionary<int, uint64>()

    let addFishInternalTimerKey () =
        [ 0 .. 9 ]
        |> List.iter (fun x -> lanternFishes.Add(x, 0UL))

    let initialiseFishCountsFromInput () =
        stateTrack
        |> List.iter (fun i -> lanternFishes.[i] <- lanternFishes.[i] + 1UL)

    let initialiseMockFishCounts () =
        lanternFishes.[3] <- 1UL
        lanternFishes.[5] <- 2UL
        
    let initialiseSpecificTests () =
        lanternFishes.[0] <- 2UL
        lanternFishes.[1] <- 1UL
        lanternFishes.[2] <- 0UL
        lanternFishes.[3] <- 0UL
        lanternFishes.[4] <- 0UL
        lanternFishes.[5] <- 1UL
        lanternFishes.[6] <- 1UL
        lanternFishes.[7] <- 1UL
        lanternFishes.[8] <- 1UL
        lanternFishes.[9] <- 2UL

    addFishInternalTimerKey ()
    initialiseFishCountsFromInput ()
//    initialiseMockFishCounts ()
    
//    initialiseSpecificTests ()

    let countFishesOverNDays n =
        [ 0 .. n ]
        |> List.iter
            (fun _ ->

                for fishAge in lanternFishes do

                    match fishAge.Key with
                    | 0 ->
                        match lanternFishes.[0] with
                        | 0UL -> lanternFishes.[0] <- 0UL
                        | x when x > 0UL ->
                            lanternFishes.[9] <- lanternFishes.[0]
                            lanternFishes.[0] <- 0UL
                        | _ -> failwith "lanternFishes.[0] edge"
                    | 7 ->
                        match lanternFishes.[7] with
                        | x when x >= 0UL ->
                            lanternFishes.[6] <- lanternFishes.[7] + lanternFishes.[9]
                            lanternFishes.[7] <- lanternFishes.[8]
                            lanternFishes.[8] <- lanternFishes.[9]
                            lanternFishes.[9] <- 0UL
                        | _ -> failwith "lanternFishes.[7] edge"
                    | x when x > 0 && x < 7 ->
                        match lanternFishes.[fishAge.Key] with
                        | 0UL -> lanternFishes.[fishAge.Key] <- 0UL
                        | x when x > 0UL ->
                            lanternFishes.[fishAge.Key - 1] <- lanternFishes.[fishAge.Key]
                            lanternFishes.[fishAge.Key] <- 0UL
                        | _ -> failwith "lanternFishes.[fishAge.Key] edge"
                    | x when x > 7 -> lanternFishes.[x] <- lanternFishes.[x]
                    | _ -> failwith "outer lanternFishes unhandled")

    countFishesOverNDays 255

    lanternFishes.Remove(9) |> ignore
    
    lanternFishes.Values |> Seq.sum


// lanternFishExponentialGrowth () 