module DayFive

open System.IO
open Microsoft.FSharp.Core

let buffer =
    seq { yield! File.ReadLines @"./Day5/actualInput.txt" }
    |> Seq.toList

let turnLineIntoStarAndEndCoordinates (str: string) =
    str.Split ' '
    |> Array.filter (fun x -> x <> "->")
    |> Array.toList

let turnIntoTuples (input: string list) =
    let intermediary =
        input
        |> List.map ((fun (x: string) -> x.Split(',')) >> Array.toList)
        |> List.collect (List.map int)

    [ (intermediary.[0], intermediary.[1])
      (intermediary.[2], intermediary.[3]) ]

let completeRanges (xs: int list, ys: int list) =
    let fullLength =

        match xs, ys with
        | [ x; x' ], [ y; y' ] when x = x' -> max y y' - min y y'
        | [ x; x' ], [ y; y' ] when y = y' -> max x x' - min x x'
        | [ x; x' ], [ y; _ ] when x > y && x > x' -> x - max x' y
        | [ x; x' ], [ y; y' ] when x > y && x > x' && x > y' -> x - x'
        | [ x; x' ], [ y; y' ] when x' > y && x' > x -> x' - min y y'
        | [ x; x' ], [ y; y' ] ->
            let maxX = max x x'
            let maxY = max y y'
            let minX = min x x'
            let minY = min y y'

            let maximum = max maxX maxY
            let minimum = min minX minY

            maximum - minimum
        | _ -> failwith "fullLength todo"

    let createList (list: int list) : int list =
        match list.[0], list.[1] with
        | x1, x2 when x1 = x2 -> List.init (fullLength + 1) (fun _ -> x1)
        | x1, x2 when x1 > x2 -> [ x1 .. -1 .. x2 ]
        | x1, x2 when x1 < x2 -> [ x1 .. x2 ]
        | _ -> failwith "createList todo"

    createList ys |> List.zip (createList xs)

let linesToProcess =
    let tuples =
        buffer
        |> List.map (
            turnLineIntoStarAndEndCoordinates
            >> turnIntoTuples
        )

    tuples
    |> List.map List.unzip
    |> List.collect completeRanges

let gridDimension =
    max (linesToProcess |> List.map fst |> List.max) (linesToProcess |> List.map snd |> List.max)
    + 1


let grid =
    seq {
        for _ in 0 .. (gridDimension - 1) do
            List.init gridDimension (fun _ -> 0)
    }
    |> Seq.toList

let increment n = n + 1

let applyToGrid (grid: int list list) (x, y) =
    let currentValue = grid.[y].[x]
    let yListWithUpdatedValue =
        (grid.[y]
        |> List.updateAt x (increment currentValue))

    grid |> List.updateAt y yListWithUpdatedValue

let answer lines =
    lines
    |> List.fold applyToGrid grid
    |> List.map (List.filter (fun x -> x >= 2))
    |> List.filter (fun x -> x <> [])
    |> List.concat
    |> List.length

// answer linesToProcess
