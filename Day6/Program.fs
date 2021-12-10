module DaySix

open System
open System.IO

type Age =
    | New
    | Old

let ageTracker i = (i, Old)

let buffer file =
    seq { yield! File.ReadLines file }
    |> Seq.head
    |> Seq.toArray
    |> Array.filter Char.IsNumber
    |> Array.map (string >> int >> ageTracker)

let state (internalTimer: int,age: Age) =
    match internalTimer with
    | 0 -> 6, Old
    | _ -> (internalTimer - 1),age
    

let lanternFishExponentialGrowth n =
    let mutable stateTrack = buffer @"/home/pk/Repos/AdventOfCode2021/Day6/exampleInput.txt"
    
    let growth currentState (newState: (int * Age)[]) =
        let zeros = currentState |> Array.filter (fun (x: int,_) -> x = 0)
        newState |> Array.append (Array.init zeros.Length (fun _ -> (8,New)))
    
    let mutable counter = n
    while counter > 0 do
        printfn $"{counter}"
        stateTrack <- growth stateTrack (stateTrack |> Array.map state)
        counter <- counter - 1
//        match counter with
//        | n when n > 0 ->
//            stateManager (counter - 1)
//        | _ -> stateTrack |> Array.length
    stateTrack
        
    
    
    
//    let stateManager' currentState nn =
//        printfn $"{nn}"
//        growth currentState (currentState |> Array.map state)
//        
//    let endResult = [0..255] |> List.fold stateManager' initialState
//    endResult.Length
        
// lanternFishExponentialGrowth (buffer @"./Day6/actualInput.txt")
// lanternFishExponentialGrowth (buffer @"./Day6/exampleInput.txt")
lanternFishExponentialGrowth 255
