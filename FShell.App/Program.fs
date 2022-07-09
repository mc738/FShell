open System
open FShell.Core

let promptHandler _ = $"[{DateTime.UtcNow:T} FShell] > "

let actionHandler (str: string) =
    match Interpreter.run str with
    | Ok steps -> Pipes.run steps
    | Error e ->
         Console.Write(Environment.NewLine)
         Console.ForegroundColor <- ConsoleColor.Red
         printfn $"Error: {e}"
         Console.ResetColor()
         1
    
let cfg =
    InputController.Common.Configuration.Create(promptHandler, actionHandler)

InputController.start cfg

printfn "Example 1"

[ CoreUtils.cat "C:\\ProjectData\\Test\\test.txt"
  CoreUtils.grep "^Hello" ]
|> Pipes.run
|> ignore

printfn "---------------------------------"
printfn "Example 2"

[ CoreUtils.ls "C:\\ProjectData\\Test"
  CoreUtils.grep ".txt$" ]
|> Pipes.run
|> ignore

printfn "---------------------------------"
printfn "Example 3"

[ CoreUtils.echo "Test error"
  CoreUtils.toError ]
|> Pipes.run
|> ignore

printfn "---------------------------------"
printfn "Example 4"
[ CoreUtils.whoami ] |> Pipes.run
|> ignore

printfn "---------------------------------"
printfn "Example 5"

[ CoreUtils.cat "C:\\Users\\44748\\Downloads\\lighthouse_preview.jpg"
  CoreUtils.base64 ]
|> Pipes.run
|> ignore