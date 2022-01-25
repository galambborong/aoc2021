module DaySeven

open System.IO

let buffer filename =
    seq { yield! File.ReadLines filename }
    |> Seq.head
    |> (fun (line: string) -> Seq.toList (line.Split ','))
    |> List.map int

let example = buffer @"./Day7/exampleInput.txt"
let actual = buffer @"./Day7/actualInput.txt"


let inc n = n + 1
5 - 1
let myWorkingExample = [0..4] |> List.map (fun x -> if x > 0 then (inc x) else 1) |> List.sum

let test arr =
    let initialPass =
        arr
        |> List.map ((+) 1)
        
    (initialPass |> List.sum) - (initialPass |> List.last)
    

let bestDifference horizontalPositions =
    let maxPosition = horizontalPositions |> List.max
    [ 0 .. maxPosition ]
    |> List.map
        (fun n ->
            horizontalPositions
            |> List.map ((fun x -> max x n - min x n) >> (fun x -> test [0..x]))
            |> List.sum
            )
    |> List.min
    
// bestDifference example
// bestDifference actual
