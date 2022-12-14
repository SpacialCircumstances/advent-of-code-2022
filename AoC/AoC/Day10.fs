module AoC.Day10

open System.IO
open FParsec


type CPUState = {
    cycle: int
    xValue: int
}

let init = {
    cycle = 1
    xValue = 1
}

type Instruction =
    | Noop
    | AddX of int

let getCycles instr = match instr with
                        | Noop -> 1
                        | AddX _ -> 2

let addInstruction = pstring "addx " >>. pint32 |>> AddX

let instruction = (pstring "noop" |>> fun _ -> Noop) <|> addInstruction

let instructions = sepEndBy instruction skipNewline

let parseInstructions text = match run instructions text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err

let incrCycle st = { st with cycle = st.cycle + 1 }

let isNoteableCycle c = (c - 20) % 40 = 0

let advanceCycles state signalStrengths cycles =
    Seq.fold (fun (st, ss) _ -> if isNoteableCycle st.cycle then (incrCycle st, (st.xValue * st.cycle) :: ss) else (incrCycle st, ss)) (state, signalStrengths) (seq { 0..(cycles - 1) })

let runInstruction (state, signalStrengths) instr =
    let (newState, newSignalStrengths) = advanceCycles state signalStrengths (getCycles instr)
    match instr with
        | Noop -> (newState, newSignalStrengths)
        | AddX dx -> ({ newState with xValue = newState.xValue + dx }, newSignalStrengths)

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day10/input.txt"
    let instructions = parseInstructions text
    let st, strengths = List.fold runInstruction (init, []) instructions
    printfn "%A" (List.sum strengths)
    ()
