module AoC.Day8

open System.IO

type Forest = {
    width: int
    height: int
    trees: int[,]
}

let rec parseForest lines =
    let height = Array.length lines
    let width = lines |> Seq.map String.length |> Seq.max
    let trees = Array2D.init width height (fun x y ->
        let line = Array.get lines y
        int (line.Substring(x, 1)))
    {
        width = width
        height = height
        trees = trees
    }

let rec right x y forest =
    if x = forest.width then
        []
    else (x, y) :: right (x + 1) y forest

let rec left x y forest =
    if x < 0 then
        []
    else (x, y) :: left (x - 1) y forest

let rec up x y forest =
    if y < 0 then
        []
    else (x, y) :: up x (y - 1) forest

let rec down x y forest =
    if y = forest.height then
        []
    else (x, y) :: down x (y + 1) forest

let markVisible forest =
    let isEdge x y = x = 0 || y = 0 || x = (forest.width - 1) || y = (forest.height - 1)
    let isHighestFromEdge x y =
        let treeH = forest.trees.[x,y]
        let treesInSightLines = [
                right (x + 1) y forest
                left (x - 1) y forest
                up x (y - 1) forest
                down x (y + 1) forest]
        let highestInSightLines = treesInSightLines |> List.map (fun sl -> List.map (fun (x, y) -> forest.trees.[x,y]) sl |> List.max)
        let lowestSightLine = List.min highestInSightLines
        treeH > lowestSightLine
    let isVisible x y = isEdge x y || isHighestFromEdge x y
    Array2D.init forest.width forest.height isVisible

let countVisible forest (visibleTrees: bool[,]) =
    let mutable c = 0
    for x in 0..(forest.width - 1) do
        for y in 0..(forest.height - 1) do
            if visibleTrees.[x,y] then
                c <- c + 1
    c

let calculateVisibilityScores forest =
    Array2D.init forest.width forest.height (fun x y ->
        let treeH = forest.trees.[x,y]
        let treesInSightLines = [
            right (x + 1) y forest
            left (x - 1) y forest
            up x (y - 1) forest
            down x (y + 1) forest ]
        let scoresInSightLines = treesInSightLines
                                 |> List.map (List.map (fun (x, y) -> forest.trees.[x,y]))
                                 |> List.map (fun sl -> (sl |> List.tryFindIndex (fun el -> el >= treeH) |> Option.map ((+) 1) |> Option.defaultValue (List.length sl)))
        List.fold (*) 1 scoresInSightLines
        )

let max2D arr =
    let mutable m = 0
    Array2D.iter (fun el -> m <- max m el) arr
    m

let solvePuzzle () =
    let lines = File.ReadAllLines("Inputs/Day8/input.txt")
    let forest = parseForest lines
    let visibleTrees = markVisible forest
    let visibleCount = countVisible forest visibleTrees
    let visibilityScores = calculateVisibilityScores forest
    let highestScore = max2D visibilityScores
    printfn $"Visible trees: %i{visibleCount}"
    printfn $"Highest visibility score: %i{highestScore}"
    ()
