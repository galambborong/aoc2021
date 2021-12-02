module DayTwo

type Instruction = {
    direction: string
    units: int
}
    
let mapInstructionToPosition currentPosition instruction =
    let (horizontal,depth) = currentPosition
    match instruction.direction, instruction.units with
    | "forward", _ -> (horizontal + instruction.units, depth)
    | "up", _ -> (horizontal, depth - instruction.units)
    | "down", _ -> (horizontal, depth + instruction.units)
    | _ -> failwith "todo"
    
let multiplyFinalPosition (x, y) = x * y
    
let dive (instructions: Instruction[]) =
    instructions
    |> Array.fold mapInstructionToPosition (0,0)
    |> multiplyFinalPosition
    
// dive exampleInput