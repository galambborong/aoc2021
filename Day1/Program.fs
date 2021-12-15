module DayOne

let countIncreases (input: int []) =
    let rec counter index count =
        match index with
        | i when i <= input.Length - 1 ->
            match input.[index], input.[index - 1] with
            | x, y when x > y -> counter (index + 1) (count + 1)
            | _ -> counter (index + 1) count
        | _ -> count

    counter 1 0

// countIncreases exampleInput
// countIncreases actualInput

let sumSlidingWindows (input: int []) =
    input |> Array.windowed 3 |> Array.map Array.sum


// countIncreases (sumSlidingWindows exampleInput)
// countIncreases (sumSlidingWindows actualInput)
