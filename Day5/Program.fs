module DayFive

open System.IO

let buffer = seq { yield! File.ReadLines @"./Day5/exampleInput.txt" }

let unwantedChars char =
    char <> ' ' || char <> '-' || char <> '>'

let turnLineIntoStarAndEndCoordinates (str: string) =
    str.Split ' '
    |> Array.filter (fun x -> x <> "->")
    |> Array.toSeq

let turnIntoTuples (input: seq<string>) =
    let intermediary = 
        input
        |> Seq.map ((fun (x: string) -> x.Split(',')) >> seq)
        |> Seq.map (Seq.map int)
        |> Seq.concat
        |> Seq.toList
    seq [(intermediary.[0], intermediary.[1]); (intermediary.[2], intermediary.[3])]

let parseBuffer =
    buffer
    |> Seq.map (turnLineIntoStarAndEndCoordinates >> turnIntoTuples)

parseBuffer