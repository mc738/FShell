namespace FShell.Core

[<RequireQualifiedAccess>]
module DisplayHandler =

    open System
    open Parsing

    let print (values: Token list) =
      values
      |> List.iter (fun t ->
          match t.Type with
          | TokenType.Command -> Console.ForegroundColor <- ConsoleColor.Blue
          | TokenType.ArgName -> Console.ForegroundColor <- ConsoleColor.Yellow
          | TokenType.ArgValue -> Console.ForegroundColor <- ConsoleColor.White
          | TokenType.Operator -> Console.ForegroundColor <- ConsoleColor.Magenta
          | TokenType.DelimitedString -> Console.ForegroundColor <- ConsoleColor.Green
          | TokenType.Text -> Console.ForegroundColor <- ConsoleColor.DarkGray
          | TokenType.Whitespace -> ()
          
          printf $"{t.Value}")
      
      Console.ResetColor()