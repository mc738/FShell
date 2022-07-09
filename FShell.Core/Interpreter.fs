namespace FShell.Core

open System
open FShell.Core.Parsing
open FShell.Core.Pipes

module Interpreter =

    type Command =
        { Name: string
          Args: CommandArg list }

        static member Blank() = { Name = ""; Args = [] }

        member cmd.SetName(name) = { cmd with Name = name }

        member cmd.AddArg(arg) = { cmd with Args = cmd.Args @ [ arg ] }

        member cmd.TryGetFirstArg() = cmd.Args |> List.tryHead

        member cmd.TryGetFirstArgValue() =
            cmd.TryGetFirstArg()
            |> Option.map (fun arg -> arg.Value)

        member cmd.TryGetArg(index: int) = cmd.Args |> List.tryItem index

        member cmd.TryGetArgValue(index: int) =
            cmd.TryGetArg index
            |> Option.map (fun arg -> arg.Value)

        member cmd.TryGetNamedArg(name: string) =
            cmd.Args
            |> List.tryFind (fun arg -> arg.Name = Some name)

        member cmd.TryGetNamedArgValue(name: string) =
            cmd.TryGetNamedArg name
            |> Option.map (fun arg -> arg.Value)

        member cmd.HasArg(name: string) =
            match cmd.TryGetNamedArg name with
            | Some _ -> true
            | None -> false

    and CommandArg =
        { Name: string option
          Value: string }

        static member Blank() = { Name = None; Value = "" }

        member arg.SetName(name) = { arg with Name = Some name }

        member arg.SetValue(value) = { arg with Value = value }

        member arg.IsEmpty() = arg.Name = None && arg.Value = ""

    type State =
        | NewCommand
        | BuildCommand of Command
        | BuildArg of Command * CommandArg

    type OperatorType =
        | Start
        | Pipe
        | FSharpPipe
        | FileRedirect
        | StdOutRedirect
        | StdErrRedirect

    type Block =
        { Command: Command
          Operator: OperatorType }

    type BuildCommandState =
        | SetName
        | BuildArg of CommandArg

    let removeQuotes (str: string) =
        match (str.StartsWith('"') || str.StartsWith(''')), (str.EndsWith('"') || str.EndsWith(''')) with
        | true, true -> str.[1..(str.Length - 2)]
        | true, false -> str.[1..]
        | false, true -> str.[0..-1] // This shouldn't be hit.
        | false, false -> str
    
    
    let buildCommand (tokens: Token list) =
        tokens
        |> List.fold
            (fun (cmd: Command, state) t ->
                match state, t.Type with
                | SetName, TokenType.Command
                | SetName, TokenType.DelimitedString -> (cmd.SetName(t.Value |> removeQuotes), BuildArg(CommandArg.Blank()))
                | SetName, _ -> failwith $"Incorrect token type: {t.Type}"
                | BuildArg arg, TokenType.ArgName -> (cmd, BuildArg(arg.SetName(t.Value)))
                | BuildArg arg, TokenType.ArgValue
                | BuildArg arg, TokenType.DelimitedString ->
                    (cmd.AddArg(arg.SetValue(t.Value |> removeQuotes)), BuildArg(CommandArg.Blank()))
                | BuildArg _, _ -> failwith $"Incorrect token type for arg: {t.Type}")
            (Command.Blank(), SetName)
        |> fun (cmd, state) ->
            match state with
            | SetName -> cmd
            | BuildArg arg when arg.IsEmpty() -> cmd
            | BuildArg arg -> cmd.AddArg(arg)

    let handleOperator (value: string) =
        match value with
        | "|>" -> FSharpPipe
        | "|" -> Pipe
        | ">" -> FileRedirect
        | ">&1" -> StdOutRedirect
        | ">&2" -> StdOutRedirect
        | _ -> failwith $"Unknown operator `{value}`."

    let split (tokens: Token list) =
        tokens
        |> List.fold
            (fun (op, acc, cur) t ->
                match t.Type with
                | Operator ->
                    handleOperator t.Value,
                    acc
                    @ [ { Command = buildCommand cur
                          Operator = op } ],
                    []
                | tt when tt.IsTrivial() -> op, acc, cur
                | _ -> op, acc, cur @ [ t ])
            (Start, [], [])
        |> fun (op, acc, cur) ->
            acc
            @ [ { Command = buildCommand cur
                  Operator = op } ]
    
    let optionToResult<'T, 'U> (errorMessage: string) (fn: 'T -> 'U) (value: Option<'T>) =
        match value with
        | Some v -> fn v |> Ok
        | None -> Error errorMessage

    let createStep (block: Block) : Result<Step, string> =

        let binder = optionToResult "Missing arg."

        match block.Operator with
        | OperatorType.Start
        | OperatorType.Pipe ->
            match block.Command.Name with
            | "ls" ->
                block.Command.TryGetFirstArgValue()
                |> Option.orElseWith (fun _ -> Some Environment.CurrentDirectory)
                |> binder CoreUtils.ls
            | "echo" ->
                block.Command.TryGetFirstArgValue()
                |> binder CoreUtils.echo
            | "whoami" -> Ok CoreUtils.whoami
            | "grep" ->
                block.Command.TryGetNamedArgValue("-p")
                |> Option.orElseWith (fun _ -> block.Command.TryGetFirstArgValue())
                |> binder CoreUtils.grep
            | "base64" -> Ok CoreUtils.base64
            | "cat" ->
                block.Command.TryGetNamedArgValue("-p")
                |> Option.orElseWith (fun _ -> block.Command.TryGetFirstArgValue())
                |> binder CoreUtils.cat
            | _ -> Error $"Unknown command `{block.Command.Name}`."
        | OperatorType.FSharpPipe -> Error "TODO - implement fsharp pipes"
        | OperatorType.FileRedirect ->
            Ok(
                CoreUtils.toFile block.Command.Name
                <| block.Command.HasArg("-p")
            )
        | OperatorType.StdErrRedirect -> Ok CoreUtils.toError
        | OperatorType.StdOutRedirect -> Error "TODO - implement stdout redirect"

    let createSteps (blocks: Block list) =
        blocks
        |> List.fold
            (fun (r: Result<Step list, string>) b ->
                r
                |> Result.bind (fun acc ->
                    match createStep b with
                    | Ok s -> acc @ [ s ] |> Ok
                    | Error e -> Error e))
            (Ok [])        
            
    let run (input: string) = Parsing.run input |> split |> createSteps