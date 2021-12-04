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


let checkRow (row: (string * bool) list) : string list option =
    match row with
    | [(_, true);(_, true);(_, true);(_, true);(_, true)] -> Some (row |> List.map fst)
    | _ -> None

// checkRow [("1", true);("99", true);("27", true);("10", true);("2", true)]
// checkRow [("1", false);("99", true);("27", true);("10", true);("2", true)]

let checkNumber bingoBoard x =
    bingoBoard
    |> List.map (applyFunctionToNestedList (matchNumber x))

let processCallNumbers (numbers: string list) (board: (string * bool) list list list) =
    numbers
    |> List.scan checkNumber board
    |> List.map (List.map (List.map checkRow))


// checkNumber "2" bingoBoard

// processCallNumbers callNumbers bingoBoard