module DayThree

open System
open System.IO


let exampleFile = @"c:/dev/aoc2021/day3/exampleInput.txt"
let actualFile = @"c:/dev/aoc2021/day3/actualInput.txt"

let fileInput file = seq { yield! File.ReadLines file }

let exampleInput = fileInput exampleFile
let actualInput = fileInput actualFile

type Rate =
    | Gamma
    | Epsilon
    
let countNumberOf1sAnd0s char =
        match char with
        | '1' -> 1
        | '0' -> 0
        | _ -> failwith "This should never occur"
    
let countBits binaryInputs index = 
        binaryInputs
        |> Seq.map (fun (binaryInput: string) -> binaryInput.[index])
        |> Seq.countBy countNumberOf1sAnd0s
        |> Seq.toList

let countPredominantBitByIndex binaryInputs (index: int) rateType =
    let count = countBits binaryInputs index
    
    match rateType with
    | Gamma -> 
        match count with
        | [(n, x); (nn, y)] when x > y ->
            n |> string
        | [(n, x); (nn, y)] when x < y ->
            nn |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"
    | Epsilon ->
        match count with
        | [(n, x); (nn, y)] when x > y ->
            nn |> string
        | [(n, x); (nn, y)] when x < y ->
            n |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"
    
    
let produceNumberFromBinary (binaryInput: seq<string>) rateType =
    let len =
        binaryInput
        |> Seq.head
        |> String.length
        |> (+) -1
        
    let binaryString =
        seq { for i in 0..len do
                  countPredominantBitByIndex binaryInput i rateType }
        |> String.concat ""
    
    Convert.ToInt32(binaryString, 2)
    
    
let calculatePowerConsumption example = produceNumberFromBinary example Gamma  * produceNumberFromBinary example Epsilon
    
    
// calculatePowerConsumption exampleInput
// calculatePowerConsumption actualInput
