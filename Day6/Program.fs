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

let state (internalTimer: int,age: Age) =
    match internalTimer with
    | 0 -> 6, Old
    | _ -> (internalTimer - 1),age
    
let growth currentState newState =
    let zeros = currentState |> List.filter (fun (x: int,_) -> x = 0)
    
    newState @ List.init zeros.Length (fun _ -> (8,New))

let lanternFishExponentialGrowth initialState =
    let rec stateManager currentState counter =
        let newState = growth currentState (currentState |> List.map state)
        match counter with
        | n when n > 0 ->
            stateManager newState (counter - 1)
        | _ -> newState |> List.length
        
    stateManager initialState 255
        
        
// lanternFishExponentialGrowth (buffer @"./Day6/actualInput.txt")
// lanternFishExponentialGrowth (buffer @"./Day6/exampleInput.txt")
