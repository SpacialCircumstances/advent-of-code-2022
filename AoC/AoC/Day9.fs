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

let follow (hx, hy) (tx, ty) =
    let delta = subV (hx, hy) (tx, ty)
    moveTail (tx, ty) delta

let rec followAll positions head =
    match List.tryHead positions with
        | Some tail ->
            let newTail = follow head tail
            newTail :: followAll (List.tail positions) newTail
        | None -> []

let moveAll (positions, visited) direction =
    let newHead = addV (List.head positions) (moveDirection direction)
    let newTails = followAll (List.tail positions) newHead
    let newPositions = newHead :: newTails
    let newTail = List.last newTails
    (newPositions, Set.add newTail visited)

let simulateMovementN commands n =
    let visitedTailPositions = Set.ofList [(0, 0)]
    let positions = List.init n (fun _ -> (0, 0))
    List.fold (fun st cmd -> List.fold (fun st _ -> moveAll st cmd.direction) st (List.init cmd.steps id)) (positions, visitedTailPositions) commands

let solvePuzzle () =
    let lines = File.ReadAllLines "Inputs/Day9/input.txt"
    let commands = parseCommands lines
    let (_, result) = simulateMovementN commands 2
    let (_, result2) = simulateMovementN commands 10
    printfn "Visited tail positions: %i" (Set.count result)
    printfn "Visited longer tail positions: %i" (Set.count result2)
    ()
