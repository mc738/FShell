namespace FShell.Core

module Parsing =

    /// Supported operators.
    let operators =
        [ "|"; "|>"; ">"; ">&1"; ">&2" ]

    /// Token types.
    type TokenType =
        | Command
        | ArgName
        | ArgValue
        | Operator
        | DelimitedString
        | Text
        | Whitespace
        
        /// Checks if a token type is trivial and can be ignored.
        member tt.IsTrivial() = match tt with Whitespace -> true | _ -> false

    /// A record representing a tokenized input value.
    type Token =
        { Type: TokenType
          Position: int
          Value: string }

        /// A catch-all method to create a token. This will handle getting the token type.
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

        /// Create a `Command` token.
        static member CreateCommand(value: string, position: int) =
            { Type = TokenType.Command
              Position = position
              Value = value }

        /// Create an `ArgName` token.
        static member CreateArgName(value: string, position: int) =
            { Type = TokenType.ArgName
              Position = position
              Value = value }

        /// Create an `ArgValue` token.
        static member CreateArgValue(value: string, position: int) =
            { Type = TokenType.ArgValue
              Position = position
              Value = value }

        /// Create an `Operator` token.
        static member CreateOperator(value: string, position: int) =
            { Type = TokenType.Operator
              Position = position
              Value = value }

        /// Create a `DelimitedString` token.
        static member CreateDelimitedString(value: string, position: int) =
            { Type = TokenType.DelimitedString
              Position = position
              Value = value }

        /// Create a `Text` token. Not currently used.
        static member CreateText(value: string, position: int) =
            { Type = TokenType.Text
              Position = position
              Value = value }

        /// Create a `Whitespace` token.
        static member CreateWhitespace(value: string, position: int) =
            { Type = TokenType.Whitespace
              Position = position
              Value = value }

    /// Run the parser and return a collection of tokens.
    let run (value: string) =

        // character to split the input on.
        let chars = [ ' '; '|'; '>'; '"'; ''' ]

        /// Create a new accumulator by checking if there is a previous substring before the current position.
        /// If so add that and the current token. If not just add the current token.
        let createNewAcc (startIndex: int) (endIndex: int) (token: Token) (isCommand: bool) (acc: Token list) =
            acc
            @ [ if endIndex > startIndex then
                    match value.GetSubString(startIndex, endIndex - 1) with
                    | s when isCommand -> Token.CreateCommand(s, startIndex)
                    | s when s.StartsWith('-') -> Token.CreateArgName(s, startIndex)
                    | s -> Token.CreateArgValue(s, startIndex)
                //Token.Create(value.GetSubString(startIndex, endIndex - 1), startIndex, isCommand)
                token ]

        /// A recursive function to handle parsing.
        let rec handle (acc: Token list, i: int, delimiter: char option, isCommand: bool) =
            let (endIndex, c) =
                value.ReadUntilChars(i, chars, delimiter)

            match c with
            // Handle whitespace.
            | Some c when c = ' ' ->

                // A bit convoluted. Essentially, is there is a prior substring (i.e. command, arg etc) and
                // isCommand is not true, pass false. If there is no prior substring then don't change is command.
                // This is ensure white spaces dont break the output.
                handle (
                    acc
                    |> createNewAcc i endIndex (Token.CreateWhitespace(" ", endIndex)) isCommand,
                    endIndex + 1,
                    None,
                    (not (endIndex > i) || not isCommand)
                    && isCommand <> false
                )
            // Handle pipe operators (`|` or `|>`)
            | Some c when c = '|' ->

                let token, newEnd =
                    match value.TryGetChar(endIndex + 1) with
                    | Some c when c = '>' -> Token.CreateOperator("|>", endIndex), endIndex + 2
                    | Some _
                    | None -> Token.CreateOperator("|", endIndex), endIndex + 1

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd, None, true)
            // Handle redirect operators (`>`, `>&1` or `>&2`).
            | Some c when c = '>' ->
                let token, newEnd =
                    match value.TryGetChar(endIndex + 1), value.TryGetChar(endIndex + 2) with
                    | Some c1, Some c2 when c1 = '&' && c2 = '1' -> Token.CreateOperator(">&1", endIndex), endIndex + 3
                    | Some c1, Some c2 when c1 = '&' && c2 = '2' -> Token.CreateOperator(">&2", endIndex), endIndex + 3
                    | Some _, _
                    | None, _ -> Token.CreateOperator(">", endIndex), endIndex + 1

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd, None, true)
            // Handle delimited strings (`"Some value"` or `'Some value'`).
            | Some c when c = '"' || c = ''' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex + 1, [ c ], None)

                let token =
                    match newC with
                    | Some _ -> Token.CreateDelimitedString(value.GetSubString(endIndex, newEnd), i)
                    | None -> Token.CreateDelimitedString(value.GetSubString(endIndex, value.Length - 1), endIndex)

                handle (acc |> createNewAcc i endIndex token isCommand, newEnd + 1, None, false)
            // This should not be hit. Throw and exception for now just to report on anytime it might be.
            | Some c ->
                failwith $"Error - unknown character `{c}`"
            // Handle end of string
            | None ->
                acc
                @ [ Token.Create(value.GetSubString(i, value.Length - 1), i, isCommand) ]

        // Parse the input into tokens.
        handle ([], 0, None, true)