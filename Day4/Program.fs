module DayFour

open System
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
    outerMostList |> List.map (List.map func)

let tuplifyBoards (gameBoards: string list list list) =
    gameBoards
    |> List.map (applyFunctionToNestedList addBoolToElement)

let bingoBoard = extractBoards |> tuplifyBoards

let matchNumber x (n: string, bool: bool) =
    match x with
    | x when x = n -> (n, true)
    | x when x <> n -> (n, bool)
    | _ -> failwith "Unhandled case"

let exampleThing = 
  [[("22", false); ("13", false); ("17", false); ("11", false); ("0", false)];
   [("8", false); ("2", false); ("23", false); ("4", false); ("24", false)];
   [("21", true); ("9", true); ("14", true); ("16", true); ("7", true)];
   [("6", false); ("10", false); ("3", false); ("18", false); ("5", false)];
   [("1", false); ("12", false); ("20", false); ("15", false); ("19", false)]]

let checkRow (row: (string * bool) list) : string list option =
    match row with
    | [ (_, true); (_, true); (_, true); (_, true); (_, true) ] -> Some(row |> List.map fst)
    | _ -> None

//let generateColumn board : string list option =
let totalBoardSum (board: (string * bool) list list) =
    board
    |> List.map (List.map (fst >> int))
    |> List.map List.sum
    |> List.sum

let checkBoard board =
    printfn "Checking board"
    let boardRow = board |> List.map checkRow

    printfn $"Matching boardRow {boardRow}"
    match boardRow with
    | [ Some list ] ->
        let boardSum = totalBoardSum board
        printfn $"Board sum: {boardSum}"
        let winningRow = list |> List.map int |> List.sum
        printfn $"Winning row: {winningRow}"
        totalBoardSum board - (list |> List.map int |> List.sum)
        
    //        (board
//         |> List.map (List.map (fst >> int))
//         |> List.map List.sum) -
//        (list
//         |> List.map int
//         |> List.sum
    | [ None ] -> 0
    | _ -> 0

// checkBoard exampleThing

let checkNumber bingoBoard x =
    bingoBoard
    |> List.map (applyFunctionToNestedList (matchNumber x))


let calculateVictory numberList callingNumber =
    numberList
    |> List.map int
    |> List.sum
    |> (*) (int callingNumber)

let playBingo (numbers: string list) boards =
    let finalCall = numbers.Length - 1

    let helperFunc board = board |> List.map (List.map checkRow)

    let rec callNumber n boards =
        match n <= finalCall with
        | true ->
            let newBoard = checkNumber boards numbers.[n]

            let checkedRows =
                (helperFunc newBoard)
                |> List.map (List.filter (fun x -> x <> None))
                |> List.concat
                |> List.head

            match checkedRows with
            | Some list -> calculateVictory list numbers.[n]
            | _ -> callNumber (n + 1) newBoard
        | _ -> failwith "This should not occur"

    callNumber 0 boards

// playBingo callNumbers bingoBoard
