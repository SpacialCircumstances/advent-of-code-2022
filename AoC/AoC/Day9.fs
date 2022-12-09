module AoC.Day9

open System.IO

type Direction =
    | Up
    | Down
    | Left
    | Right
    
let directionFromText t =
    match t with
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | _ -> failwith $"Unexpected direction: %s{t}"
    
type MoveCommand = {
    direction: Direction
    steps: int
}

type State = {
    visitedTailPositions: Set<int * int>
    tailPosition: int * int
    headPosition: int * int
}

let parseCommands (lines: string[]) =
    lines |> Seq.map (fun line ->
        let els = line.Split(' ')
        (directionFromText els.[0], int els.[1]))
        |> Seq.map (fun (dir, steps) -> { direction = dir; steps = steps })
        |> Seq.toList

let moveDirection dir = match dir with
                        | Up -> (0, 1)
                        | Down -> (0, -1)
                        | Right -> (1, 0)
                        | Left -> (-1, 0)

let addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let subV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let isTouching (x, y) = (abs x <= 1) && (abs y <= 1)

let norm v = if v = 0 then 0 else v / (abs v)

let moveTail (tx, ty) (dx, dy) =
    if isTouching (dx, dy) then
        (tx, ty)
    else
        (tx + norm dx, ty + norm dy)
        
let move state direction =
    let moveVec = moveDirection direction
    let newHead = addV state.headPosition moveVec
    let delta = subV newHead state.tailPosition
    let newTail = moveTail state.tailPosition delta
    { state with headPosition = newHead; tailPosition = newTail; visitedTailPositions = Set.add newTail state.visitedTailPositions }

let simulateMovement commands =
    let init = {
        visitedTailPositions = Set.ofList [(0, 0)]
        tailPosition = (0, 0)
        headPosition = (0, 0)
    }    
    List.fold (fun st cmd -> List.fold (fun st _ -> move st cmd.direction) st (List.init cmd.steps id)) init commands

let solvePuzzle () =
    let lines = File.ReadAllLines "Inputs/Day9/input.txt"
    let commands = parseCommands lines
    let result = simulateMovement commands
    printfn "Visited tail positions: %i" (Set.count result.visitedTailPositions)
    ()
