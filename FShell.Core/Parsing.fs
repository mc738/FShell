namespace FShell.Core

module Parsing =

    let operators =
        [ "|"; "|>"; ">"; ">&1"; ">&2" ]

    type TokenType =
        | Command
        | ArgName
        | ArgValue
        | Operator
        | DelimitedString
        | Text
        | Whitespace

    type Token =
        { Type: TokenType
          Position: int
          Value: string }

        static member Create(value: string, position: int, isCommand: bool) =
            let tt =
                match operators |> List.contains value, isCommand with
                | true, _ -> TokenType.Operator
                | false, true -> TokenType.Command
                | false, false when value.StartsWith('-') -> TokenType.ArgName
                | false, false when value.StartsWith('"') || value.StartsWith(''') -> TokenType.DelimitedString
                | false, false when value = " " -> TokenType.Whitespace
                | false, false -> TokenType.ArgValue

            { Type = tt
              Position = position
              Value = value }

    let run (value: string) =

        let chars = [ ' '; '|'; '>'; '"'; ''' ]

        let rec handle (acc: Token list, i: int, delimiter: char option, isCommand: bool) =
            let (endIndex, c) =
                value.ReadUntilChars(i, chars, delimiter)

            match c with
            | Some c when c = ' ' ->

                let newAcc =
                    acc
                    @ [ if endIndex > i then
                            Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                        Token.Create(" ", endIndex, isCommand) ]

                handle (newAcc, endIndex + 1, None, (not (endIndex > i) || not isCommand) && isCommand <> false)
            | Some c when c = '|' ->

                let token, newEnd =
                    match value.TryGetChar(endIndex + 1) with
                    | Some c when c = '>' -> Token.Create("|>", endIndex, isCommand), endIndex + 2
                    | Some _
                    | None -> Token.Create("|", endIndex, isCommand), endIndex + 1

                let newAcc =
                    acc
                    @ [ match endIndex > i with
                        | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                        | false -> ()
                        token ]

                handle (newAcc, newEnd, None, true)
            | Some c when c = '>' ->
                let token, i =
                    match value.TryGetChar(endIndex + 1), value.TryGetChar(endIndex + 2) with
                    | Some c1, Some c2 when c1 = '&' && c2 = '1' ->
                        Token.Create(">&1", endIndex, isCommand), endIndex + 3
                    | Some c1, Some c2 when c1 = '&' && c2 = '2' ->
                        Token.Create(">&2", endIndex, isCommand), endIndex + 3
                    | Some _, _
                    | None, _ -> Token.Create(">", endIndex, isCommand), endIndex + 1

                let newAcc =
                    acc
                    @ [ match endIndex > i with
                        | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                        | false -> ()
                        token ]

                handle (newAcc, i, None, true)
            | Some c when c = '"' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex + 1, [ '"' ], None)

                let newAcc =
                    match newC with
                    | Some _ ->
                        acc
                        @ [ match endIndex > i with
                            | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                            | false -> ()
                            Token.Create(value.GetSubString(endIndex, newEnd), i, isCommand) ]
                    | None ->
                        acc
                        @ [ match endIndex > i with
                            | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                            | false -> ()
                            Token.Create(value.GetSubString(endIndex, value.Length - 1), endIndex, isCommand) ]

                handle (newAcc, newEnd + 1, None, false)
            | Some c when c = ''' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex, [ ''' ], None)

                let newAcc =
                    match newC with
                    | Some _ ->
                        acc
                        @ [ match endIndex > i with
                            | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                            | false -> ()
                            Token.Create(value.GetSubString(endIndex, newEnd), endIndex, isCommand) ]
                    | None ->
                        acc
                        @ [ match endIndex > i with
                            | true -> Token.Create(value.GetSubString(i, endIndex - 1), i, isCommand)
                            | false -> ()
                            Token.Create(value.GetSubString(endIndex, value.Length - 1), endIndex, isCommand) ]

                handle (newAcc, newEnd + 1, None, false)
            | Some c -> failwith $"Error - unknown character `{c}`"
            | None ->
                acc
                @ [ Token.Create(value.GetSubString(i, value.Length - 1), i, isCommand) ]

        handle ([], 0, None, true)
