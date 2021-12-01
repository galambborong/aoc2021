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
    let sum index = input.[index..index + 2] |> Array.sum

    seq {
        for i in 0 .. (input.Length - 1) do
            sum i
    }
    |> Seq.toArray


// countIncreases (sumSlidingWindows exampleInput)
// countIncreases (sumSlidingWindows actualInput)
