module AoC.Day7

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

type FileSystemTree =
    | Directory of string * FileSystemTree list
    | File of string * int
    
type Destination =
    | Up
    | Into of string
    
type FileListing =
    | Dir of string
    | File of string * int
    
type Command =
    | Cd of Destination
    | Ls of FileListing list
    
let ws = skipMany (pchar ' ')
    
let name = satisfy (fun c -> Char.IsWhiteSpace c |> not) |> many1Chars
    
let destination = (pstring ".." |>> (fun _ -> Up)) <|> (name |>> Into)
    
let changeDirectory = pstring "cd " >>. destination .>> skipNewline |>> Cd
    
let listing = (pstring "dir " >>. ws >>. name |>> Dir)
                <|> (pint32 .>> ws .>>. name |>> (fun (size, name) -> File (name, size)))
    
let listFiles = pstring "ls" >>. ws >>. skipNewline >>. sepEndBy listing skipNewline |>> Ls
    
let command = pchar '$' >>. ws >>. (changeDirectory <|> listFiles)

let commands = many command

let parseCommands text = match run commands text with
                                | Success(res, _, _) -> res
                                | Failure (err, _, _) -> failwith err
    
let root = Directory ("/", [])
    
let parseTreeFromConsole text =
    let init = root
    let commands = parseCommands text
    List.iter (printfn "%A") commands
    ()

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day7/input.txt"
    let parsed = parseTreeFromConsole text
    ()
