module DayOne

let exampleInput =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

let countIncreases (input: int list) =
    input
    |> List.fold
        (fun acc elem ->
            printfn $"acc {acc} and elem {elem} x "
            acc + 1
            ) 0
            

// countIncreases exampleInput
