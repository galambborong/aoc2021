module DayFour

open System.IO

let readData filePath = seq { yield! File.ReadLines filePath }

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let exampleDataFilePath = @"./Day4/exampleInput.txt"
let actualDataFilePath = @"./Day4/actualInput.txt"
//let exampleInput = readData exampleDataFilePath
let exampleInput = readLines exampleDataFilePath
let actualInput = readData actualDataFilePath

let splitLineAtComma =
    (fun (line: string) -> Seq.toList (line.Split ','))

let splitLineAtSpace =
    (fun (line: string) -> Seq.toList (line.Split ' '))

let splitLineAtNewLineChar =
    (fun (line: string) -> Seq.toList (line.Split '\n'))

let emptyStrings str = str <> ""

let createCallNumberList file = file |> Seq.head |> splitLineAtComma

let callNumbers = createCallNumberList actualInput
//let callNumbers = createCallNumberList exampleInput

let createBoards file =
    file
    |> Seq.tail
    |> Seq.map splitLineAtNewLineChar
    |> List.concat
    |> List.filter emptyStrings
    |> List.map (splitLineAtSpace >> List.filter emptyStrings)
    |> List.chunkBySize 5

let extractedBoards = createBoards actualInput
//let extractedBoards = createBoards exampleInput

let addBoolToElement element = (element, false)

let applyFunctionToNestedList (func) (outerMostList: 'a list list) =
    outerMostList |> List.map (List.map func)

let tuplifyBoards (gameBoards: string list list list) =
    gameBoards
    |> List.map (applyFunctionToNestedList addBoolToElement)

let bingoBoard = extractedBoards |> tuplifyBoards

let matchNumber x (n: string, bool: bool) =
    match x with
    | x when x = n -> (n, true)
    | x when x <> n -> (n, bool)
    | _ -> failwith "Unhandled case"

let checkRow (row: (string * bool) list) : string list option =
    match row with
    | [ (_, true); (_, true); (_, true); (_, true); (_, true) ] -> Some(row |> List.map fst)
    | _ -> None

let makeColumns (board: (string * bool) list list) =
    let length = 4

    seq {
        for i in 0 .. length do
            board |> List.map (List.item i)
    }
    |> Seq.toList


let totalBoardSum (board: (string * bool) list list) =
    board
    |> List.map (List.map (fst >> int))
    |> List.map List.sum
    |> List.sum

let unmarkedSum (board: (string * bool) list list) =
    board
    |> List.map (List.filter (fun (_, y) -> y = true))
    |> List.map (List.map (fst >> int))
    |> List.map List.sum
    |> List.sum


let checkBoard (board: (string * bool) list list) =
    let boardRow =
        seq {
            for i in 0 .. (board.Length - 1) do
                board.[i] |> checkRow
        }
        |> Seq.toList
        |> List.filter (fun x -> x <> None)

    let boardColumn =
        seq {
            for i in 0 .. (board.Length - 1) do
                (makeColumns board).[i] |> checkRow
        }
        |> Seq.toList
        |> List.filter (fun x -> x <> None)

    match boardRow, boardColumn with
    | [ Some _ ], _ -> totalBoardSum board - unmarkedSum board
    | _, [ Some _ ] -> totalBoardSum board - unmarkedSum board
    | _ -> 0

let checkNumber bingoBoard x =
    bingoBoard
    |> List.map (applyFunctionToNestedList (matchNumber x))

let countTrue (board: (string * bool) list list) =
    board
    |> List.map (List.filter (fun (_, y) -> y = false))
    |> List.map List.length
    |> List.sum

let playBingo (numbers: string list) boards =
    printfn "******"
    let finalCall = numbers.Length - 1

    let rec callNumber n boards =
        printfn $"*** n = {n} ***"

        match n < finalCall with
        | true ->
            let newBoard = checkNumber boards numbers.[n]

            let checkedBoards =
                newBoard
                |> List.map checkBoard
                |> List.indexed
                |> List.filter (fun (_, y) -> y = 0)

            match checkedBoards.Length with
            | 1 ->
                printfn $"LINE 138 {n} <<<"
                callNumber (n + 1) [ newBoard.[fst checkedBoards.[0]] ]
            | _ ->
                match checkedBoards |> List.map snd with
                | x when x |> List.contains 0 -> callNumber (n + 1) newBoard
                | _ ->
                    (checkBoard newBoard.[fst checkedBoards.[0]])
                    * (numbers.[n] |> int)
        | _ -> failwith "This should not happen"

    callNumber 0 boards

// playBingo callNumbers bingoBoard
