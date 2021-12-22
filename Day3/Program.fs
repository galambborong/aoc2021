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
    | Oxygen
    | C02

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

let mostProminentBit binaryInputs (index: int) rateType =
    let count = count1sAnd0sInColumn binaryInputs index

    match rateType with
    | Gamma
    | Oxygen ->
        match count with
        | [ (n, x); (_, y) ] when x > y -> n |> string
        | [ (_, x); (nn, y) ] when x < y -> nn |> string
        | _ ->
            match rateType with
            | Oxygen -> 1 |> string
            | _ -> failwith $"{count.[0]}; {count.[1]}"
    | Epsilon
    | C02 ->
        match count with
        | [ (_, x); (nn, y) ] when x > y -> nn |> string
        | [ (n, x); (_, y) ] when x < y -> n |> string
        | _ ->
            match rateType with
            | C02 -> 0 |> string
            | _ -> failwith $"{count.[0]}; {count.[1]}"

let rec filterByCharAtIndex binaryInputs index rate =
    let charToKeep =
        mostProminentBit binaryInputs index rate |> char

    match index with
    | i when i < (binaryInputs |> Seq.head |> String.length) ->
        let filteredList =
            binaryInputs
            |> Seq.filter (fun x -> x.[index] = charToKeep)

        match filteredList |> Seq.length with
        | len when len > 1 -> filterByCharAtIndex filteredList (index + 1) rate
        | len when len = 1 -> filteredList
        | _ -> failwith "todo"
    | _ -> binaryInputs

let produceNumberFromBinary (binaryInput: seq<string>) rateType =
    let len =
        (binaryInput |> Seq.head |> String.length) - 1

    let inputIsGreaterThanOne =
        let length = binaryInput |> Seq.length
        length > 1

    let binaryString =

        match inputIsGreaterThanOne with
        | false -> binaryInput
        | true ->
            seq {
                for i in 0 .. len do
                    mostProminentBit binaryInput i rateType
            }
        |> String.concat ""

    Convert.ToInt32(binaryString, 2)


let calculatePowerConsumption example =
    produceNumberFromBinary example Gamma
    * produceNumberFromBinary example Epsilon

let calculateLifeSupport example =
    (produceNumberFromBinary (filterByCharAtIndex example 0 Oxygen) Oxygen)
    * (produceNumberFromBinary (filterByCharAtIndex example 0 C02) C02)


// calculatePowerConsumption exampleInput
// calculatePowerConsumption actualInput

// calculateLifeSupport exampleInput
// calculateLifeSupport actualInput
