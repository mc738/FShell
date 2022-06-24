namespace FShell.Core

module InputControl =

    open System

    [<AutoOpen>]
    module Common =

        type Configuration =
            private
                { PromptHandler: unit -> string
                  ActionHandler: string -> unit }

            static member Create(promptFn: unit -> string, actionFn: string -> unit) =
                { PromptHandler = promptFn
                  ActionHandler = actionFn }
            
            member cfg.GetPrompt() = cfg.PromptHandler()

            member cfg.ExecuteAction(str: string) = cfg.ActionHandler str
        
        type State =
            { CurrentPosition: int
              CurrentLine: int
              MaxPosition: int
              LeftBuffer: char list
              RightBuffer: char list
              History: string list
              HistoryIndex: int option
              SavedInput: string option }

            static member Create() =
                { CurrentPosition = 0
                  CurrentLine = 0
                  MaxPosition = 0
                  LeftBuffer = []
                  RightBuffer = []
                  History = []
                  HistoryIndex = None
                  SavedInput = None }

            member state.MoveBack1() =

                { state with
                    CurrentPosition = state.CurrentPosition - 1
                    LeftBuffer = state.LeftBuffer.[0 .. (state.LeftBuffer.Length - 2)]
                    RightBuffer =
                        [ state.LeftBuffer.[(state.LeftBuffer.Length - 1)] ]
                        @ state.RightBuffer
                    CurrentLine = state.CurrentLine }

            member state.MoveForwards1() =
                let newLeft, newRight =
                    match state.RightBuffer.IsEmpty with
                    | true -> state.LeftBuffer, []
                    | false -> state.LeftBuffer @ [ state.RightBuffer.Head ], state.RightBuffer.Tail

                { state with
                    CurrentPosition = state.CurrentPosition + 1
                    LeftBuffer = newLeft
                    RightBuffer = newRight
                    CurrentLine = state.CurrentLine }

            member state.Backspace1() =
                { state with
                    CurrentPosition = state.CurrentPosition - 1
                    MaxPosition = state.MaxPosition - 1
                    LeftBuffer = state.LeftBuffer.[0 .. (state.LeftBuffer.Length - 2)]
                    CurrentLine = state.CurrentLine }

            member state.Delete1() =
                { state with
                    MaxPosition = state.MaxPosition - 1
                    RightBuffer = state.RightBuffer.Tail
                    CurrentLine = state.CurrentLine }

            member state.AddChar(c: Char) =
                { state with
                    CurrentPosition = state.CurrentPosition + 1
                    LeftBuffer = state.LeftBuffer @ [ c ]
                    MaxPosition = state.MaxPosition + 1 }

            member state.GetString() =
                String(
                    (state.LeftBuffer @ state.RightBuffer)
                    |> Array.ofList
                )

            member state.IsEmpty() =
                state.LeftBuffer.Length = 0
                && state.RightBuffer.Length = 0

            member state.GetLeftBufferString() =
                String(state.LeftBuffer |> Array.ofList)

            member state.GetRightBufferString() =
                String(state.RightBuffer |> Array.ofList)

            member state.NextLine(offset: int, newHistoryItem: string option) =

                { state with
                    CurrentPosition = 0
                    CurrentLine = state.CurrentLine + offset
                    MaxPosition = 0
                    LeftBuffer = []
                    RightBuffer = []
                    History =
                        newHistoryItem
                        |> Option.map (fun nhi ->
                            match state.History.IsEmpty with
                            | true -> [ nhi ]
                            | false ->
                                match state.History.Head = nhi with
                                | true -> state.History
                                | false -> [ nhi ] @ state.History)
                        |> Option.defaultValue state.History
                    HistoryIndex = None
                    SavedInput = None }

            member state.LoadHistoryItem(index: int, saveInput: bool) =
                let item = state.History.[index]

                { state with
                    CurrentPosition = item.Length
                    MaxPosition = item.Length
                    LeftBuffer = item |> List.ofSeq
                    RightBuffer = []
                    HistoryIndex = Some index
                    SavedInput =
                        match saveInput with
                        | true -> Some <| state.GetString()
                        | false -> state.SavedInput }

            member state.LoadSavedInput() =
                let savedInput =
                    state.SavedInput
                    |> Option.defaultValue String.Empty

                { state with
                    CurrentPosition = savedInput.Length
                    MaxPosition = savedInput.Length
                    LeftBuffer = savedInput |> List.ofSeq
                    RightBuffer = []
                    HistoryIndex = None
                    SavedInput = None }

    [<AutoOpen>]
    module private Actions =

        let clearCurrentLine (prompt: string) _ =
            Console.Write(String(' ', Console.WindowWidth - 1 - prompt.Length))

        let setPosition (prompt: string) (state: State) =
            Console.CursorLeft <- state.CurrentPosition + prompt.Length
            Console.CursorTop <- state.CurrentLine

        let setStartOfLine (prompt: string) (state: State) =
            Console.CursorLeft <- prompt.Length
            Console.CursorTop <- state.CurrentLine

        let updateLine (prompt: string) (state: State) =
            Console.CursorVisible <- false
            Console.CursorLeft <- prompt.Length
            clearCurrentLine prompt ()
            setStartOfLine prompt state

            Console.Write(state.GetLeftBufferString())
            Console.Write(state.GetRightBufferString())
            Console.CursorVisible <- true

        let writePrompt (prompt: string) (state: State) =
            Console.CursorTop <- state.CurrentLine
            Console.CursorLeft <- 0
            Console.Write prompt

    [<RequireQualifiedAccess>]
    module KeyHandlers =

        let leftArrow (prompt: string) (state: State) =
            match (state.CurrentPosition <> 0) with
            | true ->
                // Transfer the head of right buffer into tail of left buffer.
                let newState = state.MoveBack1()
                //updateLine prompt newState
                newState
            | false ->
                // Return state as if at 0 (no op).
                state

        let rightArrow (state: State) =
            match state.CurrentPosition < state.MaxPosition with
            | true -> state.MoveForwards1()
            | false -> state

        let upArrow (prompt: string) (state: State) =
            match state.History.IsEmpty, state.HistoryIndex with
            | true, _ -> state
            | false, None ->
                let newState =
                    state.LoadHistoryItem(0, true)

                updateLine prompt newState
                newState
            | false, Some i when i < state.History.Length - 1 ->
                let newState =
                    state.LoadHistoryItem(i + 1, false)

                updateLine prompt newState
                newState
            | false, Some _ -> state

        let downArrow (prompt: string) (state: State) =
            match state.HistoryIndex with
            | Some i when i = 0 ->
                let newState = state.LoadSavedInput()
                updateLine prompt newState
                newState
            | Some i ->
                let newState =
                    state.LoadHistoryItem(i - 1, false)

                updateLine prompt newState
                newState
            | None -> state

        let backspace (prompt: string) (state: State) =
            match state.CurrentPosition > 0 with
            | true ->
                let newState = state.Backspace1()
                updateLine prompt newState
                newState
            | false -> state

        let delete (prompt: string) (state: State) =
            match state.CurrentPosition < state.MaxPosition with
            | true ->
                let newState = state.Delete1()
                updateLine prompt newState
                newState
            | false -> state

        let otherKey (prompt: string) (c: char) (state: State) =
            let newState = state.AddChar c
            updateLine prompt newState
            Console.CursorLeft <- newState.CurrentPosition
            newState

    let rec handleInput (prompt: string, state: State) =

        // Set the position to make sure everything displays correctly.
        setPosition prompt state
        let input = Console.ReadKey(true)

        match input.Key with
        | ConsoleKey.Enter -> state
        | ConsoleKey.LeftArrow -> handleInput (prompt, KeyHandlers.leftArrow prompt state)
        | ConsoleKey.RightArrow -> handleInput (prompt, KeyHandlers.rightArrow state)
        | ConsoleKey.UpArrow -> handleInput (prompt, KeyHandlers.upArrow prompt state)
        | ConsoleKey.DownArrow -> handleInput (prompt, KeyHandlers.downArrow prompt state)
        | ConsoleKey.Backspace -> handleInput (prompt, KeyHandlers.backspace prompt state)
        | ConsoleKey.Delete -> handleInput (prompt, KeyHandlers.delete prompt state)
        | _ -> handleInput (prompt, KeyHandlers.otherKey prompt input.KeyChar state)

    let rec run (cfg: Configuration, state: State) =
        let prompt = cfg.GetPrompt()
        
        writePrompt prompt state

        let result = handleInput (prompt, state)

        let newState =
            match result.IsEmpty() with
            | true -> state.NextLine(1, None)
            | false ->
                let v = result.GetString()
                cfg.ExecuteAction v
                state.NextLine(2, Some v)

        run (cfg, newState)

    let start (cfg: Configuration) =

        Console.Clear()
        let initState = State.Create()
        run (cfg, initState)