module DayFive

open System.IO
open Microsoft.FSharp.Core

let buffer =
    seq { yield! File.ReadLines @"./Day5/exampleInput.txt" }

type Coordinate = Coordinate of x: int * y: int

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

let completeRanges (xs: int list, ys: int list) =
    let fullLength =
        match xs, ys with
        | [ x; x' ], [ y; y' ] when x = x' -> max y y' - min y y'
        | [ x; x' ], [ y; y' ] when y = y' -> max x x' - min x x'
        | [ x; x' ], [ y; _ ] 
            when x > y && x > x' -> x - max x' y
        | _ -> failwith "fullLength todo"

    let createList (list: int list) : int list =
        match list.[0], list.[1] with
        | x1, x2 when x1 = x2 -> List.init (fullLength + 1) (fun _ -> x1)
        | x1, x2 when x1 > x2 ->
            [ x1 .. -1 .. x2 ]
        | x1, x2 when x1 < x2 -> [ x1 .. x2 ]
        | _ -> failwith "createList todo"

    createList ys
    |> List.zip (createList xs)

let parseBuffer () =
    let tuples =
        buffer
        |> Seq.map (
            turnLineIntoStarAndEndCoordinates
            >> turnIntoTuples
        )

    seq {
        for coordinateSet in tuples do
            coordinateSet |> Seq.toList |> List.unzip
    }
//    |> Seq.toList
    |> Seq.map completeRanges
//    |> List.concat
    


let linesToProcess = parseBuffer ()

// linesToProcess
