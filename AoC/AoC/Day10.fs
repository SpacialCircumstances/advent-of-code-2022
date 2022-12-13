module AoC.Day10

open System.IO
open FParsec


type Instruction =
    | Noop
    | AddX of int

let addInstruction = pstring "addx " >>. pint32 |>> AddX

let instruction = (pstring "noop" |>> fun _ -> Noop) <|> addInstruction

let instructions = sepEndBy instruction skipNewline

let parseInstructions text = match run instructions text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day10/input.txt"
    let instructions = parseInstructions text
    printfn "%A" instructions
    ()
