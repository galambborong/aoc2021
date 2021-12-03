module DayThree

open System.IO

let exampleInput = @"c:/dev/aoc2021/day3/datainputs.txt"

let fileInput file = seq { yield! File.ReadLines file }

let countBits binaryInputs index = 
        binaryInputs
        |> Seq.map (fun (binaryInput: string) -> binaryInput.[index])
        |> Seq.countBy (fun x ->
            match x with
            | '1' -> 1
            | '0' -> 0
            | _ -> failwith "This should never occur"
            )
        |> Seq.toList

let countPredominantBitByIndex binaryInputs (index: int) isMax =
    let count = countBits binaryInputs index
    
    match isMax with
    | true -> 
        match count with
        | [(n, x); (nn, y)] when x > y ->
            n |> string
        | [(n, x); (nn, y)] when x < y ->
            nn |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"
    | false ->
        match count with
        | [(n, x); (nn, y)] when x > y ->
            nn |> string
        | [(n, x); (nn, y)] when x < y ->
            n |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"
    
    
let possibleFinal (binaryInput: seq<string>) isMax =
    seq { for i in 0..4 do
              countPredominantBitByIndex binaryInput i isMax }
    |> String.concat ""
    
    
// countPredominantBitByIndex (fileInput exampleInput) 2
// countPredominantBitByIndex (fileInput exampleInput) 1
// countPredominantBitByIndex (fileInput exampleInput) 0

// possibleFinal (fileInput exampleInput) false

