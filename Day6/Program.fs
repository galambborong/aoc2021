module DaySix

open System
open System.IO



let buffer file =
    seq { yield! File.ReadLines file }
    |> Seq.head
    |> Seq.toArray
    |> Array.filter Char.IsNumber
    |> Array.map (string >> int)

let state (internalTimer: int) =
    match internalTimer with
    | 0 -> 6
    | _ -> internalTimer - 1
    

let lanternFishExponentialGrowth n =
    let mutable stateTrack = buffer @"./Day6/exampleInput.txt"
    
    let growth currentState (newState: int []) =
        let zeros = currentState |> Array.filter (fun (x: int) -> x = 0)
        newState |> Array.append (Array.init zeros.Length (fun _ -> 8))
    
    let mutable counter = n
    while counter > 0 do
        printfn $"counter: {counter} -- length: {stateTrack.Length}"
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
let x = lanternFishExponentialGrowth 256 |> Array.length
