module DayFour

open System.IO

let readData filePath = seq { yield! File.ReadLines filePath }
let exampleDataFilePath = @"./Day4/exampleInput.txt"
let exampleInput = readData exampleDataFilePath

let splitLineAtComma =
    (fun (line: string) -> Seq.toList (line.Split ','))

let splitLineAtSpace =
    (fun (line: string) -> Seq.toList (line.Split ' '))

let splitLineAtNewLineChar =
    (fun (line: string) -> Seq.toList (line.Split '\n'))

let emptyStrings str = str <> ""

let callNumbers =
    exampleInput |> Seq.head |> splitLineAtComma

let extractBoards =
    exampleInput
    |> Seq.tail
    |> Seq.map splitLineAtNewLineChar
    |> List.concat
    |> List.filter emptyStrings
    |> List.map (splitLineAtSpace >> List.filter emptyStrings)
    |> List.chunkBySize 5

let addBoolToElement element = (element, false)

let applyFunctionToNestedList (func) (outerMostList: 'a list list) =
    outerMostList
    |> List.map (List.map func)

let tuplifyBoards (gameBoards: string list list list) =
    gameBoards
    |> List.map (applyFunctionToNestedList addBoolToElement)

let bingoBoard = extractBoards |> tuplifyBoards

let matchNumber x (n, bool) =
    printfn $"x = {x}, tuple = ({n}, {bool})"
    match x with
    | x when x = n -> (n, true)
    | x when x <> n -> (n, bool)
    | _ -> failwith "Unhandled case"

//let isBingo =
//   let checkRows row =
        

let checkNumber bingoBoard x =
    bingoBoard
    |> List.map (applyFunctionToNestedList (matchNumber x))

let processCallNumbers numbers board =
    numbers
    |> List.scan checkNumber board


// checkNumber "2" bingoBoard

// processCallNumbers callNumbers bingoBoard