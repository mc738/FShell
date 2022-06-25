namespace FShell.Core

[<RequireQualifiedAccess>]
module DisplayHandler =

    open System
        
    type PrintState =
    | Command
    | Arg

    let operators = [ "|"; "|>"; ">"; ">&1"; ">&2" ]

    let print (values: string list) =
      values
      |> List.fold (fun ps s ->
          match s, ps with
          | s, _ when operators |> List.contains s ->
              Console.ForegroundColor <- ConsoleColor.Magenta
              printf $"{s}"
              PrintState.Command
          | s, _ when s = " " ->
              printf $"{s}"
              ps
          | s, PrintState.Command ->
              Console.ForegroundColor <- ConsoleColor.Blue
              printf $"{s}"
              PrintState.Arg
          | s, PrintState.Arg when s.StartsWith('-') ->
              Console.ForegroundColor <- ConsoleColor.Yellow
              printf $"{s}"
              PrintState.Arg
          | s, PrintState.Arg when s.StartsWith ('"') ->
              Console.ForegroundColor <- ConsoleColor.Green
              printf $"{s}"
              PrintState.Arg
          | s, PrintState.Arg ->
              Console.ForegroundColor <- ConsoleColor.White
              printf $"{s}"
              PrintState.Arg) (PrintState.Command)
          
      |> ignore
      Console.ResetColor()

