module AoC.Day7

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

type FileSystemTree =
    | Directory of string * FileSystemTree list
    | FileEntry of string * int
    
type Destination =
    | Up
    | Into of string
    
type FileListing =
    | Dir of string
    | File of string * int
    
type Command =
    | Cd of Destination
    | Ls of FileListing list
    
type State = {
    fs: FileSystemTree list
    getters: (unit -> FileSystemTree list) list
    modifiers: ((FileSystemTree list -> FileSystemTree list) -> State -> State) list
}
    
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
    
let getDir name listings = listings |> List.pick (fun d -> match d with
                                                            | Directory (n, l) when n = name -> Some l
                                                            | _ -> None)
    
let updateDir name listings update = listings |> List.map (fun l -> match l with
                                                                        | Directory (n, s) when n = name -> Directory (n, update s)
                                                                        | x -> x)
    
let updateState state listing =
    let currentModi = List.head state.modifiers
    match listing with
        | Dir name -> currentModi (fun ls -> Directory (name, []) :: ls) state
        | File (name, size) -> currentModi (fun ls -> FileEntry (name, size) :: ls) state
    
let applyCommand state cmd =
    match cmd with
        | Cd dest ->
            match dest with
                | Up ->
                    { state with getters = List.tail state.getters; modifiers = List.tail state.modifiers }
                | Into dirName ->
                    let currGet = List.head state.getters
                    let getter = fun () -> getDir dirName (currGet ())
                    let currMod = List.head state.modifiers
                    let modi = fun mf -> currMod (fun f -> updateDir dirName f mf)
                    { state with getters = getter :: state.getters; modifiers = modi :: state.modifiers }
        | Ls listings ->
            List.fold updateState state listings
    
let parseTreeFromConsole text =
    let commands = parseCommands text
    let init = {
        fs = [root]
        getters = [fun () -> [root]]
        modifiers = [fun mf st -> { st with fs = mf st.fs } ]
    }
    let finishedState = List.fold applyCommand init commands
    finishedState.fs

let rec getSize entry =
    match entry with
        | FileEntry (_, size) -> size
        | Directory (_, files) -> files |> List.map getSize |> List.sum

let rec foldFSTree folder init tree =
    List.fold (fun st entry -> match entry with
                                | FileEntry _ -> folder st entry
                                | Directory (_, subtree) ->
                                    foldFSTree folder (folder st entry) subtree) init tree

let summing sum entry =
    match entry with
        | FileEntry _ -> sum
        | Directory _ ->
            let size = getSize entry
            if size < 100000 then
                sum + size
            else sum

let solvePuzzle () =
    let text = File.ReadAllText "Inputs/Day7/input.txt"
    let parsed = parseTreeFromConsole text
    let sumOfSizesOfBelow100000 = foldFSTree summing 0 parsed
    printfn $"%i{sumOfSizesOfBelow100000}"
