namespace FShell.Core

module InputController =

    open System

    /// Common components for use through the module.
    [<AutoOpen>]
    module Common =

        /// The input controller configuration for handling the prompts and actions.
        type Configuration =
            private
                { PromptHandler: unit -> string
                  ActionHandler: string -> unit }

            /// Create a new InputController.Configuration
            static member Create(promptFn: unit -> string, actionFn: string -> unit) =
                { PromptHandler = promptFn
                  ActionHandler = actionFn }

            /// Get shell prompt string.
            member cfg.GetPrompt() = cfg.PromptHandler()

            /// Execute the configuration's action.
            member cfg.ExecuteAction(str: string) = cfg.ActionHandler str

        /// Represents the controller's internal state
        type State =
            { CurrentPosition: int
              CurrentLine: int
              MaxPosition: int
              LeftBuffer: char list
              RightBuffer: char list
              History: string list
              HistoryIndex: int option
              SavedInput: string option }

            /// Create a new blank state.
            static member Create() =
                { CurrentPosition = 0
                  CurrentLine = 0
                  MaxPosition = 0
                  LeftBuffer = []
                  RightBuffer = []
                  History = []
                  HistoryIndex = None
                  SavedInput = None }
                            
            /// Get the current left and right buffers as a string.
            member state.GetString() =
                String(
                    (state.LeftBuffer @ state.RightBuffer)
                    |> Array.ofList
                )

            /// Checks if the left and right buffers are empty.
            member state.IsEmpty() =
                state.LeftBuffer.Length = 0
                && state.RightBuffer.Length = 0

            /// Get the left buffer as a string.
            member state.GetLeftBufferString() =
                String(state.LeftBuffer |> Array.ofList)

            /// Get the right buffer as string.
            member state.GetRightBufferString() =
                String(state.RightBuffer |> Array.ofList)
                            
            /// A a new character to the end of the left buffer.
            /// A new state will always be returned.
            member state.AddChar(c: Char) =
                { state with
                    CurrentPosition = state.CurrentPosition + 1
                    LeftBuffer = state.LeftBuffer @ [ c ]
                    MaxPosition = state.MaxPosition + 1 }
                
            /// Load item at an index from the history.
            /// A new state will always be returned.
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
                
            /// Load the saved input.
            /// A new state will always be returned.
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

            /// Move to the next (based on offset) line and optionally add an item to the history.
            /// A new state will always be returned.
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

            /// Attempt to move back 1 position.
            /// If successful a new state will be returned, if not None will be.
            member state.TryMoveBack1() =
                match (state.CurrentPosition <> 0) with
                | true ->
                    { state with
                        CurrentPosition = state.CurrentPosition - 1
                        LeftBuffer = state.LeftBuffer.[0 .. (state.LeftBuffer.Length - 2)]
                        RightBuffer =
                            [ state.LeftBuffer.[(state.LeftBuffer.Length - 1)] ]
                            @ state.RightBuffer
                        CurrentLine = state.CurrentLine }
                    |> Some
                | false -> None

            /// Move forwards 1 position.
            /// If successful a new state will be returned, if not None will be.
            member state.TryMoveForwards1() =
                match state.CurrentPosition < state.MaxPosition with
                | true ->
                    let newLeft, newRight =
                        match state.RightBuffer.IsEmpty with
                        | true -> state.LeftBuffer, []
                        | false -> state.LeftBuffer @ [ state.RightBuffer.Head ], state.RightBuffer.Tail

                    { state with
                        CurrentPosition = state.CurrentPosition + 1
                        LeftBuffer = newLeft
                        RightBuffer = newRight
                        CurrentLine = state.CurrentLine }
                    |> Some
                | false -> None

            /// Attempt to remove the last character in the left buffer.
            /// If successful a new state will be returned, if not None will be.
            member state.TryBackspace1() =
                match state.CurrentPosition > 0 with
                | true ->
                    { state with
                        CurrentPosition = state.CurrentPosition - 1
                        MaxPosition = state.MaxPosition - 1
                        LeftBuffer = state.LeftBuffer.[0 .. (state.LeftBuffer.Length - 2)]
                        CurrentLine = state.CurrentLine }
                    |> Some
                | false -> None

            /// Attempt to remove the first character from the right buffer.
            /// If successful a new state will be returned, if not None will be.
            member state.TryDelete1() =
                match state.CurrentPosition < state.MaxPosition with
                | true ->
                    { state with
                        MaxPosition = state.MaxPosition - 1
                        RightBuffer = state.RightBuffer.Tail
                        CurrentLine = state.CurrentLine }
                    |> Some
                | false -> None

            /// Attempt to load the next item from the state's history
            /// If successful a new state will be returned, if not None will be.
            member state.TryNextHistoryItem() =
                match state.History.IsEmpty, state.HistoryIndex with
                | true, _ -> None
                | false, None -> Some <| state.LoadHistoryItem(0, true)
                | false, Some i when i < state.History.Length - 1 -> Some <| state.LoadHistoryItem(i + 1, false)
                | false, Some _ -> None

            /// Attempt to love the previous item from history, or the saved input if already at the first index (0).
            /// If successful a new state will be returned, if not None will be.
            member state.TryPreviousHistoryItem() =
                match state.HistoryIndex with
                | Some i when i = 0 -> Some <| state.LoadSavedInput()
                | Some i -> Some <| state.LoadHistoryItem(i - 1, false)
                | None -> None


    [<AutoOpen>]
    module private Actions =

        /// Clear the current line (minus the prompt).
        let clearCurrentLine (prompt: string) _ =
            Console.Write(String(' ', Console.WindowWidth - 1 - prompt.Length))

        /// Set the console cursor position based on a state.
        let setPosition (prompt: string) (state: State) =
            Console.CursorLeft <- state.CurrentPosition + prompt.Length
            Console.CursorTop <- state.CurrentLine

        /// Set to the start of writeable line (i.e. after the prompt).
        let setStartOfLine (prompt: string) (state: State) =
            Console.CursorLeft <- prompt.Length
            Console.CursorTop <- state.CurrentLine

        /// Update a line based on the state.
        let updateLine (prompt: string) (state: State) =
            Console.CursorVisible <- false
            Console.CursorLeft <- prompt.Length
            clearCurrentLine prompt ()
            setStartOfLine prompt state

            Console.Write(state.GetLeftBufferString())
            Console.Write(state.GetRightBufferString())
            Console.CursorVisible <- true

        /// Write the prompt.
        let writePrompt (prompt: string) (state: State) =
            Console.CursorTop <- state.CurrentLine
            Console.CursorLeft <- 0
            Console.Write prompt

    [<RequireQualifiedAccess>]
    module KeyHandlers =

        /// Handle the left arrow key (i.e. attempt to move back one).
        /// If unsuccessful this is a no-op.
        let leftArrow (state: State) =
            state.TryMoveBack1() |> Option.defaultValue state

        /// Handle the right arrow key (i.e. attempt to move forwards one).
        /// If unsuccessful this is a no-op.
        let rightArrow (state: State) =
            state.TryMoveForwards1() |> Option.defaultValue state

        /// Handle the up arrow key (i.e. load the next item from the state's history).
        /// If unsuccessful this is a no-op.
        let upArrow (prompt: string) (state: State) =
            match state.TryNextHistoryItem() with
            | Some newState ->
                updateLine prompt newState
                newState
            | None -> state

        /// Handle the down arrow key (i.e. load last time from the state's history).
        /// If unsuccessful this is a no-op.
        let downArrow (prompt: string) (state: State) =
            match state.TryPreviousHistoryItem() with
            | Some newState ->
                updateLine prompt newState
                newState
            | None -> state

        /// Handle the backspace (i.e. remove one behind current position).
        /// If unsuccessful this is a no-op.
        let backspace (prompt: string) (state: State) =
            match state.TryBackspace1() with
            | Some newState ->
                updateLine prompt newState
                newState
            | None -> state

        /// Handle the delete key (i.e. remove one forwards from current position).
        /// If unsuccessful this is a no-op.
        let delete (prompt: string) (state: State) =
            match state.TryDelete1() with
            | Some newState ->
                updateLine prompt newState
                newState
            | None -> state

        /// Handle any other key (i.e. add to the state).
        /// This always creates a new state.
        let otherKey (prompt: string) (c: char) (state: State) =
            let newState = state.AddChar c
            updateLine prompt newState
            Console.CursorLeft <- newState.CurrentPosition
            newState

    let rec handleInput (prompt: string, state: State) =

        // Set the position to make sure everything displays correctly.
        setPosition prompt state
        
        // Wait for the user to press a key.
        let input = Console.ReadKey(true)

        match input.Key with
        | ConsoleKey.Enter -> state
        | ConsoleKey.LeftArrow -> handleInput (prompt, KeyHandlers.leftArrow state)
        | ConsoleKey.RightArrow -> handleInput (prompt, KeyHandlers.rightArrow state)
        | ConsoleKey.UpArrow -> handleInput (prompt, KeyHandlers.upArrow prompt state)
        | ConsoleKey.DownArrow -> handleInput (prompt, KeyHandlers.downArrow prompt state)
        | ConsoleKey.Backspace -> handleInput (prompt, KeyHandlers.backspace prompt state)
        | ConsoleKey.Delete -> handleInput (prompt, KeyHandlers.delete prompt state)
        | _ -> handleInput (prompt, KeyHandlers.otherKey prompt input.KeyChar state)

    /// Wait for the user to input a line and then handle it.
    let rec handleLine (cfg: Configuration, state: State) =
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

        handleLine (cfg, newState)

    /// Start the input controller.
    let start (cfg: Configuration) =

        Console.Clear()
        let initState = State.Create()
        handleLine (cfg, initState)
