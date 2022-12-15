module AoC.Day11

open System.IO
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
    items: int list
    operation: Expr
    test: Test
    number: int
}

type State = {
    monkeys: Monkey list
    inspectionCounters: Map<int, int>
}

let initState monkeys = {
    monkeys = monkeys
    inspectionCounters = List.map (fun m -> (m.number, 0)) monkeys |> Map.ofList
}

let monkeyHeader = pstring "Monkey " >>. pint32 .>> pchar ':' .>> spaces

let startingItems = pstring "Starting items: " >>. sepBy pint32 (pchar ',' .>> spaces) .>> spaces

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
    items = si
    number = mh
}

let monkeys = many1 monkey

let parseMonkeys text = match run monkeys text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err

let inspect operation item =
    let getValue op = match op with
                        | Number i -> i
                        | Old -> item
                        
    let (op1, oper, op2) = operation
    let v1 = getValue op1
    let v2 = getValue op2
    match oper with
        | Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2

let bore worryLevel = worryLevel / 3

let performTest test worryLevel =
    if worryLevel % test.divisibleBy = 0 then test.ifTrueMonkey else test.ifFalseMonkey

let incrCounter inspectionCounters idx = Map.change idx (Option.map ((+) 1)) inspectionCounters

let performItem (operation, test) monkeyIdx state item =
    let worryLevel = item |> inspect operation |> bore
    let targetMonkeyIdx = performTest test worryLevel
    let targetMonkey = List.item targetMonkeyIdx state.monkeys
    let newMonkey = { targetMonkey with items = targetMonkey.items @ [worryLevel] }
    { state with monkeys = List.updateAt targetMonkeyIdx newMonkey state.monkeys; inspectionCounters = incrCounter state.inspectionCounters monkeyIdx }

let performTurn state monkeyIdx =
    let monkey = state.monkeys.[monkeyIdx]
    let turnState = { state with monkeys = List.updateAt monkeyIdx { monkey with items = [] } state.monkeys }
    List.fold (performItem (monkey.operation, monkey.test) monkeyIdx) turnState (monkey.items)

let performRound state round =
    let monkeyCount = List.length state.monkeys
    Seq.fold performTurn state (seq { 0..(monkeyCount - 1) })

let performRounds monkeys rounds =
    let st = initState monkeys
    Seq.fold performRound st (seq { 0..(rounds - 1) })

let calculateBusiness state =
    let top2 = state.inspectionCounters |> Map.toList |> List.map snd |> List.sortDescending |> List.take 2
    (List.item 0 top2) * (List.item 1 top2)

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day11/input.txt"
    let monkeys = parseMonkeys text
    let endState = performRounds monkeys 20
    let business = calculateBusiness endState
    printfn $"%A{business}"
    ()
