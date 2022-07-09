namespace FShell.Core

open System

/// Module containing the basic pipe implementation.
module Pipes =

    open System
    open System.IO
    
    /// A step in a pipeline. Takes a byte array (stdIn) and returns a byte array,
    /// either for stdOut or stdErr. 
    type Step = byte array -> Result<byte array, byte array>

    /// Run a individual step by reading from stdIn and either outputting to stdOut or stdErr.
    /// Returns a result indicating if the step was successful. 
    let runStep (step: Step) (stdIn: MemoryStream) (stdOut: MemoryStream) (stdErr: MemoryStream) =
        match stdIn.TryReadToEnd() with
        | Ok b ->
            match step b with
            | Ok r ->
                stdOut.Write r
                Ok()
            | Error e ->
                stdErr.Write e
                Error()
        | Error e ->
            stdErr.WriteString e
            Error()

    /// Run a pipeline (a collection of steps).
    let run (steps: Step list) =

        // Create memory streams for stdIn, stdOut and stdErr
        use stdIn = new MemoryStream()
        use stdOut = new MemoryStream()
        use stdErr = new MemoryStream()

        // On success, copy the contents of stdOut to stdIn,
        // for use in the next step.
        let onSuccess () =
            stdOut.Reset()
            stdIn.Clear()
            stdOut.CopyTo(stdIn)
            stdOut.Clear()
            stdIn.Reset()

        // Get a result from a memory stream,
        // for use with stdOut and stdErr.
        let getResult (ms: MemoryStream) =
            ms.Reset()
            match ms.TryReadToString() with
            | Ok s -> s
            | Error _ -> String.Empty
        
        // Output a string to the console if not null or empty.
        let printIfNotEmpty (str: string) =
            match  String.IsNullOrWhiteSpace str with
            | true -> 0
            | false ->
                Console.Write(Environment.NewLine)
                let lines = str.Split(Environment.NewLine)
                lines |> Array.iter (fun l -> printfn $"{l}")
                lines.Length
        
        // Run the steps and get the result.
        // If a step fails no steps afterward will be run.
        let result =
            steps
            // This is basically a foldi folder - i just tracks the current items index.
            |> List.fold
                (fun (r, i) s ->
                    match r with
                    | Ok _ ->
                        match runStep s stdIn stdOut stdErr with
                        | Ok _ ->
                            // If not the last step run onSuccess to copy stdOut to stdIn.
                            // This is mainly done so the streams are not disposed.
                            if i <> steps.Length - 1 then onSuccess ()
                            Ok(), i + 1
                        | Error _ ->
                            // If the result is an error do nothing.
                            // runStep will already of handled writing to stdErr.
                            Error(), i + 1
                    | Error _ ->
                        // Pipeline failed at an earlier step,
                        // do nothing.
                        r, i + 1)
                (Ok(), 0)
            // Discard i, this is not needed.
            |> fun (r, _) -> r
        
        // Handle the result.
        match result with
        | Ok _ ->
            getResult stdOut |> printIfNotEmpty
        | Error _ ->
            Console.ForegroundColor <- ConsoleColor.Red
            let offset = getResult stdErr |> printIfNotEmpty
            Console.ResetColor()
            offset