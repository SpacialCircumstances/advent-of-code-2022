module AoC.Day10

open System
open System.IO
open FParsec


type ScreenState = {
    row: int
    col: int
    canvas: char[][]
}

let initScreen w h = {
    row = 0
    col = 0
    canvas = Array.init h (fun _ -> Array.create w ' ')
}

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

let isInSprite col spritePos = col = spritePos - 1 || col = spritePos || col = spritePos + 1

let drawTick st sc =
    let spritePos = st.xValue
    let charToDraw = if isInSprite sc.col spritePos then '#' else '.'
    sc.canvas[sc.row][sc.col] <- charToDraw
    let (nr, nc) = if sc.col = 39 then (sc.row + 1, 0) else (sc.row, sc.col + 1)
    { sc with row = nr; col = nc }

let advanceCycle (st, sc, ss) _ =
    if isNoteableCycle st.cycle then
        (incrCycle st, drawTick st sc, (st.xValue * st.cycle) :: ss)
    else (incrCycle st, drawTick st sc, ss)

let advanceCycles (state, screen, signalStrengths) cycles =
    Seq.fold advanceCycle (state, screen, signalStrengths) (seq { 0..(cycles - 1) })

let runInstruction (state, screen, signalStrengths) instr =
    let (newState, newScreen, newSignalStrengths) = advanceCycles (state, screen, signalStrengths) (getCycles instr)
    match instr with
        | Noop -> (newState, newScreen, newSignalStrengths)
        | AddX dx -> ({ newState with xValue = newState.xValue + dx }, newScreen, newSignalStrengths)

let printCanvas screen =
    for row in screen.canvas do
        printfn $"%s{String row}"
    ()

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day10/input.txt"
    let instructions = parseInstructions text
    let st, screen, strengths = List.fold runInstruction (init, initScreen 40 6, []) instructions
    printfn "%A" (List.sum strengths)
    printCanvas screen
    ()
