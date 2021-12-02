module DayTwo

type Instruction = { direction: string; units: int }

let mapInstructionToPosition currentPosition instruction =
    let horizontal, depth, aim = currentPosition

    match instruction.direction with
    | "forward" -> (horizontal + instruction.units, depth + (aim * instruction.units), aim)
    | "up" -> (horizontal, depth, aim - instruction.units)
    | "down" -> (horizontal, depth, aim + instruction.units)
    | _ -> failwith "This case should never occur"

let multiplyFinalPosition (x, y, _) = x * y

let dive (instructions: Instruction []) =
    instructions
    |> Array.fold mapInstructionToPosition (0, 0, 0)
    |> multiplyFinalPosition

// dive exampleInput
// dive actualInput
