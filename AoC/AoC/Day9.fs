﻿module AoC.Day9

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
        
let move (headPosition, tailPosition, visitedTailPositions) direction =
    let moveVec = moveDirection direction
    let newHead = addV headPosition moveVec
    let delta = subV newHead tailPosition
    let newTail = moveTail tailPosition delta
    (newHead, newTail, Set.add newTail visitedTailPositions)

let simulateMovement2 commands =
    let visitedTailPositions = Set.ofList [(0, 0)]
    let tailPosition = (0, 0)
    let headPosition = (0, 0)
    let init = (headPosition, tailPosition, visitedTailPositions) 
    List.fold (fun st cmd -> List.fold (fun st _ -> move st cmd.direction) st (List.init cmd.steps id)) init commands

let solvePuzzle () =
    let lines = File.ReadAllLines "Inputs/Day9/input.txt"
    let commands = parseCommands lines
    let (_, _, result) = simulateMovement2 commands
    printfn "Visited tail positions: %i" (Set.count result)
    ()