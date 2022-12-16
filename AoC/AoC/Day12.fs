module AoC.Day12

open System.IO

type Heightmap = {
    width: int
    height: int
    heights: byte[,]
    target: int * int
    start: int * int
}

let rec parseHeightmap lines =
    let height = Array.length lines
    let width = lines.[0] |> String.length
    let heights = Array2D.create width height 0uy
    
    let mutable start = None
    let mutable target = None
    
    for x in 0..(width - 1) do
        for y in 0..(height - 1) do
            let c = lines.[y].Chars x
            let h = match c with
                    | 'S' ->
                        start <- Some (x, y)
                        0uy
                    | 'E' ->
                        target <- Some (x, y)
                        25uy
                    | a -> (int a) - 97 |> byte
            heights.[x,y] <- h
    {
        width = width
        height = height
        heights = heights
        target = target |> Option.get
        start = start |> Option.get
    }

let inMap heightmap (x, y) = x >= 0 && y >= 0 && x < heightmap.width && y < heightmap.height

let canMove hf ht = ht <= hf + 1uy

let swap x = fun a b -> x b a

let possibleFrom heightmap (fx, fy) visited =
    let heightF = heightmap.heights[fx, fy]
    [
        (fx + 1, fy)
        (fx, fy + 1)
        (fx - 1, fy)
        (fx, fy - 1)
    ]
    |> List.filter (inMap heightmap)
    |> List.filter (fun p -> Set.contains p visited |> not)
    |> List.filter (fun (px, py) -> canMove heightF (heightmap.heights[px, py]))
    
let rec findShortestPathR heightmap froms target visited len =
    let (nexts, newVisited) = List.mapFold (fun v n ->
                    let nexts = possibleFrom heightmap n v
                    (nexts, List.fold (Set.add |> swap) v nexts)) visited froms
    let nexts = List.collect id nexts
    if List.isEmpty nexts then
        None
    elif List.contains target nexts then
         Some (len + 1)
    else findShortestPathR heightmap nexts target newVisited (len + 1)
   
let findShortestPath heightmap start = findShortestPathR heightmap [start] heightmap.target (Set.singleton heightmap.start) 0
    
let findZeroPoints heightmap =
    seq {
        for x in 0..(heightmap.width - 1) do
            for y in 0..(heightmap.height - 1) do
                if heightmap.heights[x, y] = 0uy then
                    yield (x, y)
    }
    
let solvePuzzle () =
    let text = File.ReadAllLines "Inputs/Day12/input.txt" |> Array.map (fun s -> s.Trim())
    let heightmap = parseHeightmap text
    let shortestPath = findShortestPath heightmap heightmap.start |> Option.get
    printfn $"Puzzle1: %i{shortestPath}"
    let pointsAtHeightZero = findZeroPoints heightmap
    let shortestPaths = Seq.choose (findShortestPath heightmap) pointsAtHeightZero
    printfn $"Puzzle2: %i{Seq.min shortestPaths}"
    ()
