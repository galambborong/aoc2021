module DaySeven

open System.IO

let buffer filename =
    seq { yield! File.ReadLines filename }
    |> Seq.head
    |> (fun (line: string) -> Seq.toList (line.Split ','))
    |> List.map int

let example = buffer @"./Day7/exampleInput.txt"
let actual = buffer @"./Day7/actualInput.txt"

let totalTravel travel =
    let incrementTravelByOne = travel |> List.map ((+) 1)

    (incrementTravelByOne |> List.sum)
    - (incrementTravelByOne |> List.last)
    
let difference a b =
    max a b - min a b

let bestDifference horizontalPositions =
    let maxPosition = horizontalPositions |> List.max

    [ 0 .. maxPosition ]
    |> List.map
        (fun n ->
            horizontalPositions
            |> List.map (
                fun x -> totalTravel [ 0 .. (difference x n) ]
            )
            |> List.sum)
    |> List.min

// bestDifference example
// bestDifference actual
