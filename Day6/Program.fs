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
    
let growth currentState (newState: (int * Age)[]) =
    let zeros = currentState |> Array.filter (fun (x: int,_) -> x = 0)
    newState |> Array.append (Array.init zeros.Length (fun _ -> (8,New)))

let lanternFishExponentialGrowth  =
    let mutable stateTrack = buffer @"./Day6/exampleInput.txt"
    let rec stateManager counter =
        printfn $"{counter}"
        stateTrack <- growth stateTrack (stateTrack |> Array.map state)
        match counter with
        | n when n > 0 ->
            stateManager (counter - 1)
        | _ -> stateTrack |> Array.length
        
    
    stateManager 255
    
    
//    let stateManager' currentState nn =
//        printfn $"{nn}"
//        growth currentState (currentState |> Array.map state)
//        
//    let endResult = [0..255] |> List.fold stateManager' initialState
//    endResult.Length
        
// lanternFishExponentialGrowth (buffer @"./Day6/actualInput.txt")
// lanternFishExponentialGrowth (buffer @"./Day6/exampleInput.txt")
