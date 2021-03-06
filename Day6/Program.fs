module DaySix

open System
open System.Collections.Generic
open System.IO

let buffer file =
    seq { yield! File.ReadLines file }
    |> Seq.head
    |> Seq.toList
    |> List.filter Char.IsNumber
    |> List.map (string >> int)

let lanternFishExponentialGrowth n =
    let initialFishes = buffer @"./Day6/actualInput.txt"

    let lanternFishes = Dictionary<int, uint64>()

    let addFishInternalTimerKeys () =
        [ 0 .. 9 ]
        |> List.iter (fun x -> lanternFishes.Add(x, 0UL))

    let initialiseFishCountsFromInput () =
        initialFishes
        |> List.iter (fun i -> lanternFishes.[i] <- lanternFishes.[i] + 1UL)

    let countFishesOverNDays () =
        [ 0 .. (n - 1) ]
        |> List.iter
            (fun _ ->
                for fishAge in lanternFishes do
                    match fishAge.Key with
                    | 0 ->
                        lanternFishes.[9] <- lanternFishes.[0]
                        lanternFishes.[0] <- 0UL
                    | x when x > 0 && x < 7 ->
                        lanternFishes.[fishAge.Key - 1] <- lanternFishes.[fishAge.Key]
                        lanternFishes.[fishAge.Key] <- 0UL
                    | 7 ->
                        lanternFishes.[6] <- lanternFishes.[7] + lanternFishes.[9]
                        lanternFishes.[7] <- lanternFishes.[8]
                        lanternFishes.[8] <- lanternFishes.[9]
                        lanternFishes.[9] <- 0UL
                    | x when x > 7 -> lanternFishes.[x] <- lanternFishes.[x]
                    | _ -> failwith "outer lanternFishes unhandled")

    addFishInternalTimerKeys ()

    initialiseFishCountsFromInput ()

    countFishesOverNDays ()

    lanternFishes.Remove(9) |> ignore

    lanternFishes.Values |> Seq.sum


// lanternFishExponentialGrowth 80
// lanternFishExponentialGrowth 256
