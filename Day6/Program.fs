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
    
    
let countZeros (dict: Dictionary<int, int>) =
    dict.Values

let lanternFishExponentialGrowth () =
    let stateTrack = buffer @"/home/pk/Repos/AdventOfCode2021/Day6/exampleInput.txt"
   
    let mutable dict = Dictionary<int, int>()
    let mutable inter = Dictionary<int,int>()
            
    for i in 0..stateTrack.Length - 1 do
        dict.Add(i, stateTrack.[i])
            
    for i in 0..79 do
        for ob in dict do
            dict.[ob.Key] <- state ob.Value
            if dict.[ob.Key] = 0 then do
                inter.[dict.Count + inter.Count] <- 8
            
        for ob in inter do
            dict[ob.Key] <- ob.Value
            
        inter.Clear()
    
    dict.Count

//lanternFishExponentialGrowth()
