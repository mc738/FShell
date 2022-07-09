namespace FShell.Core

[<AutoOpen>]
module Extensions =

    open System
    open System.IO
    open System.Text

    type MemoryStream with

        /// Try and read to then end the stream.
        /// If the stream length is longer than Int32.MaxValue this will fail.
        member ms.TryReadToEnd() =
            match ms.Length < (int64 Int32.MaxValue) with
            | true ->
                let buffer =
                    Array.zeroCreate (int ms.Length)

                ms.Read(Span buffer) |> ignore
                Ok buffer
            | false -> Error "Stream is bigger that 2.14748365 gb (2147483647 bytes). Can not read to end."

        /// Try and read a string from the stream.
        member ms.TryReadToString() =
            ms.TryReadToEnd()
            |> Result.map Encoding.UTF8.GetString

        /// Write a string to the stream.
        member ms.WriteString(value: string) =
            let b = value |> Encoding.UTF8.GetBytes
            ms.Write(ReadOnlySpan b)
        
        /// Reset the position of the stream.
        member ms.Reset() = ms.Position <- 0L

        /// Clear the stream.
        member ms.Clear() =
            ms.Reset()
            let len = int ms.Length
            ms.Write(Array.zeroCreate len, 0, len)
            ms.Reset()
            
    type String with
    
        /// Check if an index in a string is in bounds.
        member s.InBounds(i: int) = i >= 0 && i < s.Length
        
        /// Try and get a character from a string by index (if in bounds)
        member s.TryGetChar(i) =
            match s.InBounds i with
            | true -> s.[i] |> Some
            | false -> None
            
        /// Read from the string until one of a selection of characters are found.
        /// Supports an optional delimiter character.
        /// If present, will only return the next applicable character index that is not delimited.
        /// For example in single or double quotes.
        member s.ReadUntilChar(start: int, chars: char list, delimiter: char option) =
            let rec read(i: int, delimited: bool) =
                match s.TryGetChar i, delimiter with
                | Some c, Some delimiter ->
                    match c = delimiter, delimited, chars |> List.contains c with
                    | true, true, _ -> read (i + 1, false)
                    | true, false, _ -> read (i + 1, true)
                    | false, _, true -> (i, Some c)
                    | false, _, false -> read (i + 1, delimited)
                | Some c, None ->
                    match chars |> List.contains c with
                    | true -> (i, Some c)
                    | false -> read (i + 1, false)
                | None, _ ->
                    // Read to end and out of bounds, so return the last value.
                    (i, None)
            
            read(start, false)
            
        /// Get a substring starting at startIndex and ending at endIndex.
        member s.GetSubString(startIndex: int, endIndex: int) =
            s.[startIndex..endIndex]
            
        /// Attempts to remove quotes (' or ") from the start and end of a string.
        member s.RemoveQuotes() =
            match (s.StartsWith('"') || s.StartsWith(''')), (s.EndsWith('"') || s.EndsWith(''')) with
            | true, true -> s.[1..(s.Length - 2)]
            | true, false -> s.[1..]
            | false, true -> s.[0..(s.Length - 2)]
            | false, false -> s
    