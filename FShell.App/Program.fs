open System
open FShell.Core

let promptHandler _ = $"[{DateTime.UtcNow:T} FShell] > "

let actionHandler (str: string) =
    Console.Write(Environment.NewLine)
    printfn $"{str}"

let cfg =
    InputControl.Common.Configuration.Create(promptHandler, actionHandler)

InputControl.start cfg

printfn "Example 1"

[ CoreUtils.cat "C:\\ProjectData\\Test\\test.txt"
  CoreUtils.grep "^Hello" ]
|> Pipes.run

printfn "---------------------------------"
printfn "Example 2"

[ CoreUtils.ls "C:\\ProjectData\\Test"
  CoreUtils.grep ".txt$" ]
|> Pipes.run

printfn "---------------------------------"
printfn "Example 3"

[ CoreUtils.echo "Test error"
  CoreUtils.toError ]
|> Pipes.run

printfn "---------------------------------"
printfn "Example 4"
[ CoreUtils.whoami ] |> Pipes.run

printfn "---------------------------------"
printfn "Example 5"

[ CoreUtils.cat "C:\\Users\\44748\\Downloads\\lighthouse_preview.jpg"
  CoreUtils.base64 ]
|> Pipes.run
