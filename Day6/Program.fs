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
    |> Seq.toList
    |> List.filter Char.IsNumber
    |> List.map (string >> int >> ageTracker)

let state (internalTimer,age) =
    match (internalTimer, age) with
    | 0, Old -> 6, Old
    | 0, New -> 8, Old
    | _, age -> internalTimer - 1, age
    
let growth currentState newState =
    match currentState, newState with
    | currentState, _ when currentState |> List.contains (0, Old) -> newState |> List.append [(8, New)]
    | _ -> newState

let lanternFishExponentialGrowth initialState =
    let rec stateManager currentState counter =
        printfn $"counter: {counter}"
        let newState = currentState |> List.map state
        let newNewState = growth currentState newState
        match counter with
        | n when n > 0 ->
            stateManager newNewState (counter - 1)
        | _ -> currentState |> List.map state |> List.map fst
        
    stateManager initialState 80
        
        
    
// lanternFishExponentialGrowth (buffer @"./Day6/exampleInput.txt")