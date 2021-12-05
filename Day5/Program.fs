module DayFive

open System.IO

let buffer =
    seq { yield! File.ReadLines @"./Day5/exampleInput.txt" }

let turnLineIntoStarAndEndCoordinates (str: string) =
    str.Split ' '
    |> Array.filter (fun x -> x <> "->")
    |> Array.toSeq

let turnIntoTuples (input: seq<string>) =
    let intermediary =
        input
        |> Seq.map ((fun (x: string) -> x.Split(',')) >> seq)
        |> Seq.collect (Seq.map int)
        |> Seq.toList

    seq [ (intermediary.[0], intermediary.[1])
          (intermediary.[2], intermediary.[3]) ]
    
    
// produce seq per start/end pair
// flatten sequences
// map coordinate to +1 on grid
// (grid inits all 0s)

let parseBuffer =
    buffer
    |> Seq.map (
        turnLineIntoStarAndEndCoordinates
        >> turnIntoTuples
    )

// parseBuffer
