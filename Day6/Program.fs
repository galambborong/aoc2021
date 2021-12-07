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

// buffer @"./Day6/exampleInput.txt"


let state (internalTimer,age) =
    match (internalTimer, age) with
    | 0, Old -> 6
    | 0, New -> 8
    | _ -> internalTimer - 1
    
let growth x =
    match x with
    | 0 -> Some (List.init 0 (fun _ -> 0))
    | _ -> None

let lanternFishExponentialGrowth initialState =
    let rec stateManager currentState counter =
        match counter with
        | n when n > 0 ->
            let newState = currentState |> List.map state
            stateManager newState (counter - 1)
        | _ -> currentState
        
    stateManager initialState 80
        
        
    
// lanternFishExponentialGrowth (buffer @"./Day6/exampleInput.txt")