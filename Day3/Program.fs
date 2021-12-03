module DayThree

open System
open System.IO


let exampleFile = @"./Day3/exampleInput.txt"
let actualFile = @"./Day3/actualInput.txt"

let fileInput file = seq { yield! File.ReadLines file }

let exampleInput = fileInput exampleFile
let actualInput = fileInput actualFile

type Rate =
    | Gamma
    | Epsilon

let oneOrZero char =
    match char with
    | '1' -> 1
    | '0' -> 0
    | _ -> failwith "This should never occur"

let count1sAnd0sInColumn binaryInputs index =
    binaryInputs
    |> Seq.map (fun (binaryInput: string) -> binaryInput.[index])
    |> Seq.countBy oneOrZero
    |> Seq.toList
    
let rec filterByCharAtIndex (binaryInputs: seq<string>) index charToKeep =
    match index with
    | i when i <= (binaryInputs |> Seq.head |> String.length) -> //|> (+) -1
        let filteredList = 
            binaryInputs
            |> Seq.filter (fun x -> x.[index] = charToKeep)
        match filteredList |> Seq.length with
        | len when len > 0 -> 
            filterByCharAtIndex filteredList (index + 1) charToKeep
        | _ -> binaryInputs
    | _ -> binaryInputs
    
    // filterByCharAtIndex exampleInput 0 '1'

let mostProminentBit binaryInputs (index: int) rateType =
    let count = count1sAnd0sInColumn binaryInputs index

    match rateType with
    | Gamma ->
        match count with
        | [ (n, x); (_, y) ] when x > y -> n |> string
        | [ (_, x); (nn, y) ] when x < y -> nn |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"
    | Epsilon ->
        match count with
        | [ (_, x); (nn, y) ] when x > y -> nn |> string
        | [ (n, x); (_, y) ] when x < y -> n |> string
        | _ -> failwith $"x = {fst count.[0]}; y = {fst count.[1]}"


let produceNumberFromBinary (binaryInput: seq<string>) rateType =
    let len =
        binaryInput |> Seq.head |> String.length |> (+) -1

    let binaryString =
        seq {
            for i in 0 .. len do
                mostProminentBit binaryInput i rateType
        }
        |> String.concat ""

    Convert.ToInt32(binaryString, 2)


let calculatePowerConsumption example =
    produceNumberFromBinary example Gamma
    * produceNumberFromBinary example Epsilon


// calculatePowerConsumption exampleInput
// calculatePowerConsumption actualInput
