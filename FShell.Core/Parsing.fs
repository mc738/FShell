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

    /// Run the parser and return a collection of tokens.
    let run (value: string) =

        let chars = [ ' '; '|'; '>'; '"'; ''' ]

        /// Create a new accumulator by checking if there is a previous substring before the current position.
        /// If so add that and the current token. If not just add the current token.
        let createNewAcc (startIndex: int) (endIndex: int) (token: Token) (isCommand: bool) (acc: Token list) =
            acc
            @ [ if endIndex > startIndex then
                    Token.Create(value.GetSubString(startIndex, endIndex - 1), startIndex, isCommand)
                token ]

        /// A recursive function to handle parsing.
        let rec handle (acc: Token list, i: int, delimiter: char option, isCommand: bool) =
            let (endIndex, c) =
                value.ReadUntilChars(i, chars, delimiter)

            match c with
            | Some c when c = ' ' ->

                // A bit convoluted. Essentially, is there is a prior substring (i.e. command, arg etc) and
                // isCommand is not true, pass false. If there is no prior substring then don't change is command.
                // This is ensure white spaces dont break the output.
                handle (
                    acc
                    |> createNewAcc i endIndex (Token.Create(" ", endIndex, isCommand)) isCommand,
                    endIndex + 1,
                    None,
                    (not (endIndex > i) || not isCommand)
                    && isCommand <> false
                )
            | Some c when c = '|' ->

                let token, newEnd =
                    match value.TryGetChar(endIndex + 1) with
                    | Some c when c = '>' -> Token.Create("|>", endIndex, isCommand), endIndex + 2
                    | Some _
                    | None -> Token.Create("|", endIndex, isCommand), endIndex + 1

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd, None, true)
            | Some c when c = '>' ->
                let token, newEnd =
                    match value.TryGetChar(endIndex + 1), value.TryGetChar(endIndex + 2) with
                    | Some c1, Some c2 when c1 = '&' && c2 = '1' ->
                        Token.Create(">&1", endIndex, isCommand), endIndex + 3
                    | Some c1, Some c2 when c1 = '&' && c2 = '2' ->
                        Token.Create(">&2", endIndex, isCommand), endIndex + 3
                    | Some _, _
                    | None, _ -> Token.Create(">", endIndex, isCommand), endIndex + 1

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd, None, true)
            | Some c when c = '"' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex + 1, [ '"' ], None)

                let token =
                    match newC with
                    | Some _ -> Token.Create(value.GetSubString(endIndex, newEnd), i, isCommand)
                    | None -> Token.Create(value.GetSubString(endIndex, value.Length - 1), endIndex, isCommand)

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd + 1, None, false)
            | Some c when c = ''' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex, [ ''' ], None)

                let token =
                    match newC with
                    | Some _ -> Token.Create(value.GetSubString(endIndex, newEnd), i, isCommand)
                    | None -> Token.Create(value.GetSubString(endIndex, value.Length - 1), endIndex, isCommand)

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd + 1, None, false)
            | Some c ->
                // This should not be hit. Throw and exception for now just to report on anytime it might be.
                failwith $"Error - unknown character `{c}`"
            | None ->
                acc
                @ [ Token.Create(value.GetSubString(i, value.Length - 1), i, isCommand) ]

        handle ([], 0, None, true)