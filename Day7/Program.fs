module DaySeven

open System.IO

let buffer filename =
    seq { yield! File.ReadLines filename }
    |> Seq.head
    |> (fun (line: string) -> Seq.toList (line.Split ','))
    |> List.map int

let horizontalPositions = buffer @"./Day7/actualInput.txt"

let minPosition = horizontalPositions |> List.min

let maxPosition = horizontalPositions |> List.max

let bestDifference () =
    [ 0 .. maxPosition ]
    |> List.map
        (fun n ->
            horizontalPositions
            |> List.map (fun x -> max x n - min x n)
            |> List.sum)
    |> List.min
    
// bestDifference ()
