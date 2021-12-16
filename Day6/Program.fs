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
    let stateTrack = buffer @"c:/dev/aoc2021/Day6/exampleInput.txt"
   
    let mutable dict = Dictionary<int, int>()
    let mutable inter = Dictionary<int,int>()
            
    for i in 0..stateTrack.Length - 1 do
        dict.Add(i, stateTrack.[i])
            
    for i in 0..255 do
        printfn $"{i}"
        for ob in dict do
            if dict.[ob.Key] = 0 then do
                inter.[dict.Count + inter.Count] <- 8
            dict.[ob.Key] <- state ob.Value
            
        for ob in inter do
            dict[ob.Key] <- ob.Value
            
        inter.Clear()
    
    dict.Count

// lanternFishExponentialGrowth()
