module DayFive

open System.IO
open Microsoft.FSharp.Core

let buffer =
    seq { yield! File.ReadLines @"./Day5/exampleInput.txt" }

type Coordinate =
    Coordinate of x: int * y: int
    
type CoordinateSet =
    | Start of Coordinate
    | End of Coordinate

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
//    let fullLength = max 
    let createList (list: int list) :int list =
        match list.[0], list.[1] with
        | x1, x2 when x1 = x2 -> [x1]
        | x1, x2 when x1 > x2 -> [ x2 .. x1 ]
        | x1, x2 when x1 < x2 -> [x1..x2]
        | _ -> failwith "todo"
        
    let xs' = createList xs
    let ys' = createList ys
    
    
//    match xs'.Length, ys'.Length with
//    | xs'', _ when xs'' < fullLength -> List.init fullLength (fun _ -> xs')
//    | _, ys'' when ys'' < fullLength -> List.init fullLength (fun _ -> ys')
//    | xs'', _ when xs'' = fullLength -> xs'
//    | _, ys'' when ys'' = fullLength -> ys'
    
    ys' |> List.zip xs'
    
//    seq {
//            
//        
//    }

let parseBuffer =
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
    |> Seq.map completeRanges
    
    

// parseBuffer
