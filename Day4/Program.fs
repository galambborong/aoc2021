module DayFour

open System.IO

let readData filePath = seq { yield! File.ReadLines filePath }
let exampleDataFilePath = @"./Day4/exampleInput.txt"
let actualDataFilePath = @"./Day4/actualInput.txt"
let exampleInput = readData exampleDataFilePath
let actualInput = readData actualDataFilePath

let splitLineAt (char: char) =
    (fun (line: string) -> Seq.toList (line.Split char))

let emptyStrings str = str <> ""

let createCallNumberList file = file |> Seq.head |> splitLineAt ','

let callNumbers = createCallNumberList actualInput
//let callNumbers = createCallNumberList exampleInput

let createBoards file =
    file
    |> Seq.tail
    |> Seq.map (splitLineAt '\n')
    |> List.concat
    |> List.filter emptyStrings
    |> List.map (splitLineAt ' ' >> List.filter emptyStrings)
    |> List.chunkBySize 5

let extractedBoards = createBoards actualInput
//let extractedBoards = createBoards exampleInput

let addBoolToElement element = (element, false)

let applyFunctionToNestedList func (outerMostList: 'a list list) =
    outerMostList |> List.map (List.map func)

let tuplifyBoards gameBoards =
    gameBoards
    |> List.map (applyFunctionToNestedList addBoolToElement)

let bingoBoard = extractedBoards |> tuplifyBoards

let matchNumber x (n, bool) =
    match x with
    | x when x = n -> (n, true)
    | x when x <> n -> (n, bool)
    | _ -> failwith "Unhandled case"

let checkRow row =
    match row with
    | [ (_, true); (_, true); (_, true); (_, true); (_, true) ] -> Some(row |> List.map fst)
    | _ -> None

let makeColumns board =
    seq {
        for i in 0 .. 4 do
            board |> List.map (List.item i)
    }
    |> Seq.toList


let sumOfFullBoard board =
    board
    |> List.map (List.map (fst >> int))
    |> List.map List.sum
    |> List.sum

let sumOfUnmarkedNumbers board =
    board
    |> List.map (List.filter (fun (_, y) -> y = true))
    |> sumOfFullBoard

let checkBoardForBingoSet board =
    let rowsAndColumns (board: (string * bool) list list) =
        seq { for i in 0..(board.Length - 1) do
                  board.[i] |> checkRow }
        |> Seq.toList
        |> List.filter (fun x -> x <> None)
        
    let boardRows = rowsAndColumns board
    let boardColumns = rowsAndColumns (makeColumns board)

    match boardRows, boardColumns with
    | [ Some _ ], _ -> sumOfFullBoard board - sumOfUnmarkedNumbers board
    | _, [ Some _ ] -> sumOfFullBoard board - sumOfUnmarkedNumbers board
    | _ -> 0

let markMatchingNumbersTrue bingoBoard x =
    bingoBoard
    |> List.map (applyFunctionToNestedList (matchNumber x))

let countNumberOfTruesOnBoard board =
    board
    |> List.map (List.filter (fun (_, y) -> y = false))
    |> List.map List.length
    |> List.sum

let playBingo (numbers: string list) boards =
    let finalCall = numbers.Length - 1

    let rec callNumber n boards =

        match n < finalCall with
        | true ->
            let latestBoards = markMatchingNumbersTrue boards numbers.[n]

            let boardsStillInPlay =
                latestBoards
                |> List.map checkBoardForBingoSet
                |> List.indexed
                |> List.filter (fun (_, y) -> y = 0)

            let filterBoardsWhichHaveReachedBingo checkedBoards =
                seq {
                    for board in checkedBoards do
                        latestBoards.[fst board]
                }
                |> Seq.toList


            match boardsStillInPlay.Length with
            | 1 -> callNumber (n + 1) [ latestBoards.[fst boardsStillInPlay.[0]] ]
            | 0 ->
                let indexOfLowestTrues =
                    latestBoards
                    |> List.map countNumberOfTruesOnBoard
                    |> List.indexed
                    |> List.min
                    |> fst

                checkBoardForBingoSet latestBoards.[indexOfLowestTrues]
                * (numbers.[n] |> int)
            | _ ->
                match boardsStillInPlay |> List.map snd with
                | x when x |> List.contains 0 -> callNumber (n + 1) (filterBoardsWhichHaveReachedBingo boardsStillInPlay)
                | _ -> failwith "Unhandled case"
        | _ -> failwith "This should not occur"

    callNumber 0 boards

// playBingo callNumbers bingoBoard
