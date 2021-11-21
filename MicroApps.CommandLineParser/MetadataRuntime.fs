namespace MicroApps.CommandLineParser

open Preamble

type ICommandLineMetadataProvider =
    abstract GetMetadata : unit -> Async<CommandLineMetadata>

type StaticCommandLineMetadataProvider(metadata) =
    interface ICommandLineMetadataProvider with
        member this.GetMetadata() = async.Return metadata

open Console

type internal ShowHelpOptions =
    { errors: string list
      showExamples: bool
      metadata: CommandLineMetadata
      argPath: CommandLineArgument list }

type internal BuildPlanState =
    { errors: string list
      argPath: CommandLineArgument list
      valueMap: Map<string, obj>
      tokens: ArgumentToken list }

type internal BuildPlanError =
    { errors: string list
      argPath: CommandLineArgument list
      valueMap: Map<string, obj> }

type internal BuildPlanSuccess =
    { argPath: CommandLineArgument list
      valueMap: Map<string, obj> }

type internal BuildPlanResult = Result<BuildPlanSuccess, BuildPlanError>

type CommandLineMetadataRuntime(metadataProvider: ICommandLineMetadataProvider, tokenizer: ICommandLineTokenizer) =

    let getSummaryOf desc =
        if isNotNullOrWS desc.summary then
            desc.summary
        elif isNotNullOrWS desc.name then
            desc.name
        else
            ""

    let getFormOfFlag (argDesc: ArgumentDescription) (flagDesc: FlagDescription) =
        match flagDesc.longForms with
        | first :: _ -> $"--%s{first}"
        | [] ->
            match flagDesc.shortForms with
            | first :: _ -> $"-%c{first}"
            | [] -> argDesc.key

    let getFormOfOption (argDesc: ArgumentDescription) (optDesc: OptionDescription) =
        match optDesc.longForms with
        | first :: _ -> first
        | [] ->
            match optDesc.shortForms with
            | first :: _ -> string first
            | [] -> argDesc.key

    let getSubCommandVerbs arguments =
        arguments
        |> Seq.collect
            (function
            | CommandLineVerb (_, v) -> [ v.verb ]
            | _ -> [])
        |> Seq.toList

    let showHelp (data: ShowHelpOptions) =
        let writeErrors errors =
            match errors with
            | [] -> ()
            | errors ->
                writelnC red "Errors:"

                for error in errors do
                    writelnC red $"  * %s{error}"

        let writeFocusedDesc (desc: ElementDescription) =
            if isNotNullOrWS desc.name then
                writelnC white $"%s{desc.name}"
                writeln ""

            if isNotNullOrWS desc.description then
                writelnC gray desc.description
                writeln ""
            elif isNotNullOrWS desc.summary then
                writelnC gray desc.summary
                writeln ""

        let getUsagesTable (verbDesc: VerbDescription) =
            let defaultUsages =
                seq {
                    let options =
                        verbDesc.arguments
                        |> List.collect
                            (function
                            | CommandLineFlag (_, desc, _) -> [ desc.isRequired ]
                            | CommandLineOption (_, desc, _) -> [ desc.isRequired ]
                            | _ -> [])
                        |> (function
                        | [] -> ""
                        | ls when List.exists ((=) true) ls -> " <OPTIONS>"
                        | _ -> " [OPTIONS]")

                    let arguments =
                        verbDesc.arguments
                        |> List.collect
                            (function
                            | CommandLinePositional (_, desc, _) -> [ desc.isRequired ]
                            | _ -> [])
                        |> (function
                        | [] -> ""
                        | ls when List.exists ((=) true) ls -> " <ARGUMENTS>"
                        | _ -> " [ARGUMENTS]")

                    let subCommands =
                        verbDesc.arguments
                        |> List.exists
                            (function
                            | CommandLineVerb _ -> true
                            //| CommandLineGroup _ -> true
                            | _ -> false)
                        |> (function
                        | true when Option.isNone verbDesc.execute -> " <SUB-COMMAND> ..."
                        | true -> " [SUB-COMMAND] ..."
                        | _ -> "")

                    yield
                        [ "usage:"
                          $"%s{verbDesc.verb}%s{options}%s{arguments}%s{subCommands}" ]
                }

            let usages =
                verbDesc.usages
                |> Seq.map (fun usage -> [ "or:"; usage ])

            Seq.concat [ defaultUsages; usages ] |> Seq.toList

        let writeUsages verbDesc =
            let tableOptions =
                cleanTableOptions
                |> withColumnAlignment ColumnTextAlignment.Right 0

            getUsagesTable verbDesc |> writeTable tableOptions
            writeln ""

        let getPositionalArgumentsTable arguments =
            arguments
            |> Seq.collect
                (function
                | CommandLinePositional (desc, argDesc, posDesc) -> [ desc, argDesc, posDesc ]
                | _ -> [])
            |> Seq.sortBy (fun (_, _, posDesc) -> posDesc.priority)
            |> Seq.map
                (fun (desc, argDesc, posDesc) ->
                    let key = argDesc.key

                    let multiplicity =
                        match posDesc.multiValued with
                        | None -> ""
                        | Some ({ minCount = None; maxCount = None }) -> " ..."
                        | Some ({ minCount = Some minCount
                                  maxCount = None }) -> $" %d{minCount}..."
                        | Some ({ minCount = None
                                  maxCount = Some maxCount }) -> $" ...%d{maxCount}"
                        | Some ({ minCount = Some minCount
                                  maxCount = Some maxCount }) -> $" %d{minCount}...%d{maxCount}"

                    let summary = getSummaryOf desc

                    let default' =
                        match posDesc.defaultText with
                        | Some text -> $" (Default: %s{text})"
                        | None -> ""

                    [ $"%s{key}%s{multiplicity}"
                      $"%s{summary}%s{default'}" ])
            |> Seq.toList

        let writePositionalArguments arguments =
            let tableOptions =
                { cleanTableOptions with
                      columnSeparator = "   "
                      columnSeparatorStart = "  " }
                |> withColumnMinWidth 14 0

            let table = getPositionalArgumentsTable arguments

            if table <> [] then
                writeln $"Arguments:"

            writeTable tableOptions table

            if table <> [] then writeln ""

        let getOptionArgumentsTable arguments =
            let getForms longForms shortForms =
                List.concat [ shortForms
                              |> List.map (fun (c: char) -> $"-%c{c}")
                              longForms
                              |> List.map (fun (s: string) -> $"--%s{s}") ]
                |> String.concat "|"

            let getFormatted =
                function
                | CommandLineOption (desc, argDesc, optDesc) ->
                    let forms =
                        getForms optDesc.longForms optDesc.shortForms

                    let summary = getSummaryOf desc
                    forms, summary
                | CommandLineFlag (desc, argDesc, flagDesc) ->
                    let forms =
                        getForms flagDesc.longForms flagDesc.shortForms

                    let summary = getSummaryOf desc
                    forms, summary
                | _ -> failwithf "Unexpected argument type"

            arguments
            |> Seq.collect
                (function
                | CommandLineOption (desc, argDesc, optDesc) -> [ CommandLineOption(desc, argDesc, optDesc) ]
                | CommandLineFlag (desc, argDesc, flagDesc) -> [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | _ -> [])
            |> Seq.map getFormatted
            |> Seq.map (fun (forms, summary) -> [ forms; summary ])
            |> Seq.toList

        let writeOptionArguments arguments =
            let tableOptions =
                { cleanTableOptions with
                      columnSeparator = "   "
                      columnSeparatorStart = "  " }
                |> withColumnMinWidth 14 0

            let table = getOptionArgumentsTable arguments

            if table <> [] then writeln $"Options:"

            writeTable tableOptions table

            if table <> [] then writeln ""

        let getSubcommandsTable arguments =
            arguments
            |> Seq.collect
                (function
                | CommandLineVerb (desc, verbDesc) -> [ desc, verbDesc ]
                | _ -> [])
            |> Seq.sortBy (fun (_, v) -> v.verb)
            |> Seq.map (fun (d, v) -> [ v.verb; (getSummaryOf d) ])
            |> Seq.toList

        let writeCommandsArguments arguments =
            let tableOptions =
                { cleanTableOptions with
                      columnSeparator = "   "
                      columnSeparatorStart = "  " }
                |> withColumnMinWidth 14 0

            let table = getSubcommandsTable arguments

            if table <> [] then writeln $"Commands:"

            writeTable tableOptions table

            if table <> [] then writeln ""

        let writeExamples examples =
            if data.showExamples && examples <> [] then
                writeln "Examples:"

                for example in examples do
                    writeln example
                    writeln ""

        let rec loop focused argPath =
            match argPath with
            | CommandLineVerb (desc, verbDesc) :: tail ->
                if focused then writeFocusedDesc desc
                //else
                //    writeUnfocusedDesc desc
                writeUsages verbDesc
                writePositionalArguments verbDesc.arguments
                writeOptionArguments verbDesc.arguments
                writeCommandsArguments verbDesc.arguments
                writeExamples verbDesc.examples

                loop false tail

            | _ -> ()

        writeErrors data.errors
        loop true data.argPath

    let buildPlan metadata tokens : BuildPlanResult =
        let searchArgument localOnly notFoundMsg multipleFoundMsg getMatches argPath =
            let rec loop argPath =
                match argPath with
                | [] -> []
                | CommandLineVerb (desc, verbDesc) :: argPath' ->
                    let newCandidates =
                        verbDesc.arguments
                        |> Seq.collect getMatches
                        |> Seq.toList

                    match newCandidates with
                    | [] -> if localOnly then [] else loop argPath'
                    | candidates -> candidates
                | other :: _ -> failwith $"Unexpected element %A{other}"

            loop argPath
            |> function
                | [] -> Error [ notFoundMsg ]
                | candidate :: [] -> Ok candidate
                | _ -> Error [ multipleFoundMsg ]

        let searchShortForm char =
            searchArgument
                false
                $"Short option '-%c{char}' not found"
                $"Multiple arguments use short option '-%c{char}'"
                (function
                | CommandLineFlag (desc, argDesc, flagDesc) when flagDesc.shortForms |> List.contains char ->
                    [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | CommandLineOption (desc, argDesc, optDesc) when optDesc.shortForms |> List.contains char ->
                    [ CommandLineOption(desc, argDesc, optDesc) ]
                | _ -> [])

        let searchLongForm str =
            searchArgument
                false
                $"Long option '--%s{str}' not found"
                $"Multiple arguments use long option '--%s{str}'"
                (function
                | CommandLineFlag (desc, argDesc, flagDesc) when flagDesc.longForms |> List.contains str ->
                    [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | CommandLineOption (desc, argDesc, optDesc) when optDesc.longForms |> List.contains str ->
                    [ CommandLineOption(desc, argDesc, optDesc) ]
                | _ -> [])

        let searchCommand str =
            searchArgument
                true
                $"Command '%s{str}' not found"
                $"Multiple commands use verb '%s{str}'"
                (function
                | CommandLineVerb (desc, verbDesc) when verbDesc.verb = str -> [ CommandLineVerb(desc, verbDesc) ]
                | _ -> [])

        let fillDefaultValues argPath valueMap =
            // TODO: Fill value map with default values from argPath on missing keys
            let rec loop argPath valueMap = valueMap
            loop argPath valueMap

        let searchInvalidConstraints argPath valueMap =
            // TODO: Search required options/arguments on argPath from valueMap
            // TODO: Search multi-valued options/arguments have min/max count ok
            []

        let rec loop (state: BuildPlanState) =
            let asError errors valueMap =
                Error
                    { errors = errors
                      argPath = state.argPath
                      valueMap = valueMap }

            let asOk valueMap =
                Ok
                    { argPath = state.argPath
                      valueMap = valueMap }

            match state.tokens with
            | [] -> // Whene there are no more tokens to proecss
                match state.errors with
                | [] -> // When there are no errors from looking up for tokens in metadata
                    match state.argPath with
                    | CommandLineVerb (_, verbDesc) :: _ ->
                        match verbDesc.execute with
                        | None -> // When selected verb is not executable
                            let subcommands =
                                getSubCommandVerbs verbDesc.arguments
                                |> String.concat ", "

                            let error =
                                $"Command %s{verbDesc.verb} is not executable. "
                                + $"Try one of the sub-commands: %s{subcommands}"

                            asError [ error ] state.valueMap
                        | Some _ ->
                            let valueMap =
                                fillDefaultValues state.argPath state.valueMap

                            match searchInvalidConstraints state.argPath valueMap with
                            | [] -> // When all required options are fulfilled
                                asOk valueMap
                            | errors -> // When some required options are missing
                                asError errors valueMap

                    | other :: _ -> failwith $"Unexpected element %A{other}"
                    | [] -> failwith $"Unexpected empty list"

                | errors -> // When parsing errors
                    asError (List.rev errors) state.valueMap

            | token :: tokens' ->
                let ofFlagOrOptionResult result =
                    match result with
                    | Error errors ->
                        { state with
                              errors = errors @ state.errors
                              tokens = tokens' }

                    | Ok (CommandLineFlag (desc, argDesc, flagDesc)) ->
                        match Map.tryFind argDesc.key state.valueMap with
                        | Some value ->
                            let form = getFormOfFlag argDesc flagDesc

                            let error =
                                $"Flag '%s{form}' was found multiple times"

                            { state with
                                  errors = error :: state.errors
                                  tokens = tokens' }
                        | None ->
                            { state with
                                  tokens = tokens'
                                  valueMap = state.valueMap |> Map.add argDesc.key true }

                    | Ok (CommandLineOption (desc, argDesc, optDesc)) ->
                        let readOptionValue () =
                            let rec loop tokens remaining inputs =
                                match tokens, remaining with
                                | _, n when n < 0 ->
                                    let form = getFormOfOption argDesc optDesc
                                    failwith $"Token count cannot be negative in {form}"
                                | tokens', 0 ->
                                    let inputs = List.rev inputs

                                    match optDesc.reader.read inputs with
                                    | Error error -> Error [ error ]
                                    | Ok value -> Ok(value, tokens')
                                | [], n ->
                                    let form = getFormOfOption argDesc optDesc
                                    Error [ $"Expecting %i{n} additional parameters but no more arguments were given, reading {form}" ]
                                | ValueToken str :: tokens', n -> loop tokens' (n - 1) (str :: inputs)
                                | ShortFormToken char :: _, n ->
                                    let form = getFormOfOption argDesc optDesc
                                    Error [ $"Expecting %i{n} additional parameters but found '%c{char}', reading {form}" ]
                                | LongFormToken str :: _, n ->
                                    let form = getFormOfOption argDesc optDesc
                                    Error [ $"Expecting %i{n} additional parameters but found '%s{str}', reading {form}" ]

                            loop tokens' optDesc.reader.tokenCount []

                        let accumulateValue accumFn =
                            match readOptionValue () with
                            | Error errors ->
                                { state with
                                      errors = errors @ state.errors
                                      tokens = tokens' }
                            | Ok (value, tokens') ->
                                let value' = accumFn value

                                { state with
                                      tokens = tokens'
                                      valueMap = state.valueMap |> Map.add argDesc.key value' }

                        match Map.tryFind argDesc.key state.valueMap, optDesc.multiValued with
                        | Some _, None -> // When existing value and single-valued option
                            let form = getFormOfOption argDesc optDesc

                            let error =
                                $"Option '%s{form}' was found multiple times"

                            { state with
                                  errors = error :: state.errors
                                  tokens = tokens' }
                        | None, None -> // When single-valued option
                            accumulateValue id
                        | Some value, Some _ -> // When existing values and multi-valued option
                            accumulateValue
                                (fun newValue ->
                                    match value with
                                    | :? list<obj> as accum -> accum @ [ newValue ]
                                    | other -> failwith $"Unexpected element %A{other}")
                        | None, Some _ -> // When multi-valued option
                            accumulateValue (fun value -> [ value ])

                    | Ok (other) -> failwith $"Unexpected element %A{other}"

                match token with
                | ShortFormToken char ->
                    searchShortForm char state.argPath
                    |> ofFlagOrOptionResult
                    |> loop
                | LongFormToken str ->
                    searchLongForm str state.argPath
                    |> ofFlagOrOptionResult
                    |> loop
                | ValueToken str ->
                    match searchCommand str state.argPath with
                    | Error errors ->
                        { state with
                              errors = errors @ state.errors
                              tokens = tokens' }
                        |> loop
                    | Ok (CommandLineVerb (desc, verbDesc)) ->
                        { state with
                              argPath = (CommandLineVerb(desc, verbDesc)) :: state.argPath
                              tokens = tokens' }
                        |> loop
                    | Ok other -> failwith $"Unexpected element %A{other}"

        loop
            { errors = []
              argPath = [ CommandLineVerb(metadata.desc, metadata.verb) ]
              valueMap = Map.empty
              tokens = tokens }

    let executePlan plan = async { return { exitCode = 0 } }

    let execute (context: CommandLineExecuteContext) =
        async {
            let tokenResult = tokenizer.Tokenize context.args
            let! metadata = metadataProvider.GetMetadata()

            match tokenResult with
            | Error errors ->
                showHelp
                    { errors = errors
                      showExamples = false
                      metadata = metadata
                      argPath = [ CommandLineVerb(metadata.desc, metadata.verb) ] }

                return { exitCode = 1 }
            | Ok tokens ->
                let executionPlan = buildPlan metadata tokens

                match executionPlan with
                | Error error ->
                    let showExamples =
                        match metadata.examplesFlagKey with
                        | None -> false
                        | Some key ->
                            error.valueMap
                            |> Map.tryFind key
                            |> Option.bind
                                (function
                                | :? bool as b -> Some b
                                | _ -> None)
                            |> Option.defaultValue false

                    showHelp
                        { errors = error.errors
                          showExamples = showExamples
                          metadata = metadata
                          argPath = error.argPath }

                    return { exitCode = 1 }
                | Ok plan ->
                    printfn "PLAN: %A" plan
                    let! result = executePlan plan
                    return result
        }

    interface ICommandLineRuntime with
        member this.Execute context = execute context
