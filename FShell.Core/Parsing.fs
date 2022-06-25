namespace FShell.Core


module Parsing =

    type Token =
        | Command of Value: string
        | Arg of ArgType
        | Operator of OperatorType
        | Text of Value: string
        | Whitespace

    and [<RequireQualifiedAccess>] ArgType =
        | Anonymous of Value: ArgValue
        | Named of Name: string * Value: ArgValue

    and ArgValue =
        | Plain of Value: string
        | Delimited of Value: string * Delimiter: char

    and [<RequireQualifiedAccess>] OperatorType =
        | Pipe
        | FSharpPipe
        | Redirect of RedirectTo option

        member ot.GetString() =
            match ot with
            | Pipe -> "|"
            | FSharpPipe -> "|>"
            | Redirect rt ->
                match rt with
                | Some rtt -> $">{rtt.GetString()}"
                | None -> ">"

    and [<RequireQualifiedAccess>] RedirectTo =
        | StdOut
        | StdErr

        member rt.GetString() =
            match rt with
            | StdOut -> "&1"
            | StdErr -> "$2"

    module Internal =

        ()

    let parse (value: string) =

        let chars = [ ' '; '|'; '>'; '"'; ''' ]

        let rec handle (acc: string list, i: int, delimiter: char option) =
            let (endIndex, c) =
                value.ReadUntilChars(i, chars, delimiter)

            match c with
            | Some c when c = ' ' ->

                let newAcc =
                    acc
                    @ [ if endIndex > i then
                            value.GetSubString(i, endIndex - 1)
                        " " ]

                handle (newAcc, endIndex + 1, None)
            | Some c when c = '|' ->

                let token, newEnd =
                    match value.TryGetChar(endIndex + 1) with
                    | Some c when c = '>' -> "|>", endIndex + 2
                    | Some _
                    | None -> "|", endIndex + 1

                let newAcc =
                    acc
                    @ [ match endIndex > i with
                        | true -> value.GetSubString(i, endIndex - 1)
                        | false -> ()
                        token ]

                handle (newAcc, newEnd, None)
            | Some c when c = '>' ->
                let token, i =
                    match value.TryGetChar(endIndex + 1), value.TryGetChar(endIndex + 2) with
                    | Some c1, Some c2 when c1 = '&' && c2 = '1' -> ">&1", endIndex + 3
                    | Some c1, Some c2 when c1 = '&' && c2 = '2' -> ">&2", endIndex + 3
                    | Some _, _
                    | None, _ -> ">", endIndex + 1

                let newAcc =
                    acc
                    @ [ match endIndex > i with
                        | true -> value.GetSubString(i, endIndex - 1)
                        | false -> ()
                        token ]

                handle (newAcc, i, None)
            | Some c when c = '"' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex + 1, [ '"' ], None)

                let newAcc =
                    match newC with
                    | Some _ ->
                        acc
                        @ [ match endIndex > i with
                            | true -> value.GetSubString(i, endIndex - 1)
                            | false -> ()
                            value.GetSubString(endIndex, newEnd) ]
                    | None ->
                        acc
                        @ [ match endIndex > i with
                            | true -> value.GetSubString(i, endIndex - 1)
                            | false -> ()
                            value.GetSubString(endIndex, value.Length - 1) ]

                handle (newAcc, newEnd + 1, None)
            | Some c when c = ''' ->
                let (newEnd, newC) =
                    value.ReadUntilChars(endIndex, [ ''' ], None)

                let newAcc =
                    match newC with
                    | Some _ ->
                        acc
                        @ [ match endIndex > i with
                            | true -> value.GetSubString(i, endIndex - 1)
                            | false -> ()
                            value.GetSubString(endIndex, newEnd) ]
                    | None ->
                        acc
                        @ [ match endIndex > i with
                            | true -> value.GetSubString(i, endIndex - 1)
                            | false -> ()
                            value.GetSubString(endIndex, value.Length - 1) ]

                handle (newAcc, newEnd + 1, None)
            | Some c -> failwith $"Error - unknown character `{c}`"
            | None -> acc @ [ value.GetSubString(i, value.Length - 1) ]

        handle ([], 0, None)
