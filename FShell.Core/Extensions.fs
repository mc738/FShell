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