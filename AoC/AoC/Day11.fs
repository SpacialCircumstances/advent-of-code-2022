module AoC.Day11

open System.IO
open System.Numerics
open FParsec

type Operand =
    | Old
    | Number of int

type Operator =
    | Add
    | Sub
    | Mul
    | Div

type Expr = Operand * Operator * Operand

type Test = {
    divisibleBy: int
    ifTrueMonkey: int
    ifFalseMonkey: int
}

type Monkey = {
    items: bigint list
    operation: Expr
    test: Test
    number: int
}

type State = {
    monkeys: Monkey list
    inspectionCounters: Map<int, int64>
}

let initState monkeys = {
    monkeys = monkeys
    inspectionCounters = List.map (fun m -> (m.number, 0L)) monkeys |> Map.ofList
}

let monkeyHeader = pstring "Monkey " >>. pint32 .>> pchar ':' .>> spaces

let startingItems = pstring "Starting items: " >>. sepBy pint64 (pchar ',' .>> spaces) .>> spaces

let operand = ((stringReturn "old" Old) <|> (pint32 |>> Number)) .>> spaces

let operator = ((charReturn '+' Add) <|> (charReturn '-' Sub) <|> (charReturn '*' Mul) <|> (charReturn '/' Div)) .>> spaces

let expr = operand .>>. operator .>>. operand |>> fun ((op1, ope), op2) -> (op1, ope, op2)

let operation = skipString "Operation: new = " >>. expr .>> spaces

let testDivisibleBy = skipString "divisible by" >>. spaces >>. pint32 .>> spaces

let ifTrue = skipString "If true: throw to monkey" >>. spaces >>. pint32 .>> spaces

let ifFalse = skipString "If false: throw to monkey" >>. spaces >>. pint32 .>> spaces

let test = skipString "Test: " >>. testDivisibleBy .>>. ifTrue .>>. ifFalse .>> spaces |>> fun ((divBy, ift), iff) -> {
    divisibleBy = divBy
    ifTrueMonkey = ift
    ifFalseMonkey = iff
}

let monkey = monkeyHeader .>>. startingItems .>>. operation .>>. test |>> fun (((mh, si), op), te) -> {
    operation = op
    test = te
    items = si |> List.map bigint
    number = mh
}

let monkeys = many1 monkey

let parseMonkeys text = match run monkeys text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err

let inspect operation item =
    let getValue op = match op with
                        | Number i -> bigint i
                        | Old -> item
                        
    let (op1, oper, op2) = operation
    let v1 = getValue op1
    let v2 = getValue op2
    match oper with
        | Add -> BigInteger.Add(v1, v2)
        | Sub -> BigInteger.Subtract(v1, v2)
        | Mul -> BigInteger.Multiply(v1, v2)
        | Div -> BigInteger.Divide(v1, v2)

let bore worryLevel = BigInteger.Divide(worryLevel, 3I)

let performTest test worryLevel =
    if BigInteger.DivRem(worryLevel, test.divisibleBy) |> snd = 0I then test.ifTrueMonkey else test.ifFalseMonkey

let incrCounter inspectionCounters idx = Map.change idx (Option.map ((+) 1L)) inspectionCounters

let performItem (operation, test) monkeyIdx manageWorryLevels state item =
    let worryLevel = item |> inspect operation |> manageWorryLevels
    let targetMonkeyIdx = performTest test worryLevel
    let targetMonkey = List.item targetMonkeyIdx state.monkeys
    let newMonkey = { targetMonkey with items = targetMonkey.items @ [worryLevel] }
    { state with monkeys = List.updateAt targetMonkeyIdx newMonkey state.monkeys; inspectionCounters = incrCounter state.inspectionCounters monkeyIdx }

let performTurn manageWorryLevels state monkeyIdx =
    let monkey = state.monkeys.[monkeyIdx]
    let turnState = { state with monkeys = List.updateAt monkeyIdx { monkey with items = [] } state.monkeys }
    List.fold (performItem (monkey.operation, monkey.test) monkeyIdx manageWorryLevels) turnState monkey.items

let performRound manageWorryLevels state round =
    printfn $"Performing round %i{round}"
    let monkeyCount = List.length state.monkeys
    Seq.fold (performTurn manageWorryLevels) state (seq { 0..(monkeyCount - 1) })

let performRounds manageWorryLevels monkeys rounds =
    let st = initState monkeys
    Seq.fold (performRound manageWorryLevels) st (seq { 0..(rounds - 1) })

let calculateBusiness state =
    let top2 = state.inspectionCounters |> Map.toList |> List.map snd |> List.sortDescending |> List.take 2
    (List.item 0 top2) * (List.item 1 top2)

let runPuzzleRounds monkeys rounds manageWorryLevel =
    let endState = performRounds manageWorryLevel monkeys rounds
    let business = calculateBusiness endState
    printfn "%A" endState.inspectionCounters
    printfn $"%A{business}"

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day11/test.txt"
    let monkeys = parseMonkeys text
    runPuzzleRounds monkeys 20 bore
    runPuzzleRounds monkeys 10000 id
    ()
