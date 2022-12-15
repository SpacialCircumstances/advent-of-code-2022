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
    startingItems: int list
    operation: Expr
    test: Test
    number: int
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
    startingItems = si
    number = mh
}

let monkeys = many1 monkey

let parseMonkeys text = match run monkeys text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err
let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day11/input.txt"
    let monkeys = parseMonkeys text
    printfn $"%A{monkeys}"
    ()
