module DayFive

open System.IO
open Microsoft.FSharp.Core

let buffer =
    seq { yield! File.ReadLines @"./Day5/exampleInput.txt" }
    |> Seq.toList

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

let completeRanges (xs: int list, ys: int list) =
    let fullLength =
        printfn $"xs {xs}   ys {ys}"

        match xs, ys with
        | [ x; x' ], [ y; y' ] when x = x' -> max y y' - min y y'
        | [ x; x' ], [ y; y' ] when y = y' -> max x x' - min x x'
        | [ x; x' ], [ y; _ ] when x > y && x > x' -> x - max x' y
        | [ x; x' ], [ y; y' ] when x > y && x > x' && x > y' -> x - x'
        | [ x; x' ], [ y; y' ] when x' > y && x' > x -> x' - min y y'
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

    seq {
        for coordinateSet in tuples do
            coordinateSet |> Seq.toList |> List.unzip
    }
    |> Seq.toList
    |> List.collect completeRanges

let dimension =
    let xs =
        linesToProcess |> List.map fst |> List.max

    let ys =
        linesToProcess |> List.map snd |> List.max

    max xs ys + 1


let grid =
    seq {
        for _ in 0 .. (dimension - 1) do
            List.init dimension (fun _ -> 0)
    }
    |> Seq.toList

let incrementValue n = n + 1

let applyToGrid (grid: int list list) (x, y) =
    let newY = grid.[y]
                |> List.updateAt x (incrementValue grid.[y].[x])
    
    grid |> List.updateAt y newY

let answer lines =
    lines
    |> List.fold applyToGrid grid
    
let example = [(0,9);(1,9);(2,9);(3,9);(4,9);(5,9)]

// linesToProcess