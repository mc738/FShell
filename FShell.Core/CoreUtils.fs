namespace FShell.Core

open System.Text.Unicode

/// Module containing CoreUtils. 
module CoreUtils =

    open System
    open System.IO
    open System.Text
    open System.Text.RegularExpressions    
    
    /// Helper functions for internal use.
    [<AutoOpen>]
    module private Helpers =

        /// Get a string from a byte array.
        let getString (bytes: Byte array) = Encoding.UTF8.GetString bytes

        /// Get a string array from a byte array split on a separator.
        let getStringCollection (separator: String) (bytes: Byte array) =
            getString bytes |> fun s -> s.Split separator

        /// Get bytes from a string.
        let getBytes (str: string) = Encoding.UTF8.GetBytes str

    /// A basic implementation of cat.
    let cat (path: string) _ =
        //let args = getString data

        try
            File.ReadAllBytes path |> Ok
        with
        | exn -> getBytes exn.Message |> Error

    
    /// A basic implementation of grep.
    let grep (pattern: string) (data: byte array) =

        getStringCollection Environment.NewLine data
        |> Array.choose (fun l ->
            match Regex.IsMatch(l, pattern) with
            | true -> Some l
            | false -> None)
        |> String.concat Environment.NewLine
        |> getBytes
        |> Ok

    /// A basic implementation of echo.
    let echo str _ =
        getBytes str |> Ok
        
    /// A basic implementation of ls.
    let ls (path: string) _ =
        try
            [ Directory.EnumerateDirectories(path) |> List.ofSeq
              Directory.EnumerateFiles(path) |> List.ofSeq ]
            |> List.concat
            |> fun s -> String.Join(Environment.NewLine, s)
            |> getBytes
            |> Ok
        with
        | exn -> Error (Encoding.UTF8.GetBytes exn.Message)
        
    /// Redirect a result to an error.
    let toError (bytes: byte array) = Error bytes
        
    /// A basic implementation of whoami.
    let whoami _ = getBytes Environment.UserName |> Ok
    
    /// A basic implementation of base64
    let base64 (bytes: byte array) =
        Convert.ToBase64String bytes |> getBytes |> Ok
        
    let toFile (path: string) (preserve: bool) (bytes: byte array) =
        try
            File.WriteAllBytes(path, bytes)
            match preserve with
            | true -> Ok bytes
            | false -> Ok [||]
        with
        | exn -> Error (Encoding.UTF8.GetBytes exn.Message)