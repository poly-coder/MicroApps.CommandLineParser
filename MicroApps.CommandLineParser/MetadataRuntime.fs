namespace MicroApps.CommandLineParser

open Preamble

type ICommandLineMetadataProvider =
    abstract GetMetadata : unit -> Async<CommandLineMetadata>

type StaticCommandLineMetadataProvider(metadata) =
    interface ICommandLineMetadataProvider with
        member this.GetMetadata() = async.Return metadata

open Console
open System

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
    { execute: ExecuteVerbFunc
      metadata: CommandLineMetadata
      argPath: CommandLineArgument list
      valueMap: Map<string, obj> }

type internal BuildPlanResult = Result<BuildPlanSuccess, BuildPlanError>

type CommandLineMetadataRuntime
    (
        metadataProvider: ICommandLineMetadataProvider,
        tokenizer: ICommandLineTokenizer,
        serviceProvider: IServiceProvider
    ) =

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

        let writeParentDesc (desc: ElementDescription) =
            if isNotNullOrWS desc.name then
                writelnC white $"%s{desc.name}"
                writeln ""

        let (|HasNoOptions|HasRequiredOptions|HasOptions|) arguments =
            arguments
            |> List.collect
                (function
                | CommandLineFlag (_, desc, _) -> [ desc.isRequired ]
                | CommandLineOption (_, desc, _) -> [ desc.isRequired ]
                | _ -> [])
            |> (function
            | [] -> HasNoOptions
            | ls when List.exists ((=) true) ls -> HasRequiredOptions
            | _ -> HasOptions)

        let (|HasNoArguments|HasRequiredArguments|HasArguments|) arguments =
            arguments
            |> List.collect
                (function
                | CommandLinePositional (_, desc, _) -> [ desc.isRequired ]
                | _ -> [])
            |> (function
            | [] -> HasNoArguments
            | ls when List.exists ((=) true) ls -> HasRequiredArguments
            | _ -> HasArguments)

        let (|HasNoSubCommands|HasRequiredSubCommands|HasSubCommands|) verbDesc =
            verbDesc.arguments
            |> List.exists
                (function
                | CommandLineVerb _ -> true
                //| CommandLineGroup _ -> true
                | _ -> false)
            |> (function
            | true when Option.isNone verbDesc.execute -> HasRequiredSubCommands
            | true -> HasSubCommands
            | _ -> HasNoSubCommands)

        let getUsagesTable verbChainName (verbDesc: VerbDescription) =
            let defaultUsages =
                seq {
                    let options =
                        match verbDesc.arguments with
                        | HasNoOptions -> ""
                        | HasRequiredOptions -> " <OPTIONS>"
                        | HasOptions -> " [OPTIONS]"

                    let arguments =
                        match verbDesc.arguments with
                        | HasNoArguments -> ""
                        | HasRequiredArguments -> " <ARGUMENTS>"
                        | HasArguments -> " [ARGUMENTS]"

                    let subCommands =
                        match verbDesc with
                        | HasNoSubCommands -> ""
                        | HasRequiredSubCommands -> " <SUB-COMMAND>"
                        | HasSubCommands -> " [SUB-COMMAND]"

                    yield
                        [ "usage:"
                          $"%s{verbChainName}%s{options}%s{arguments}%s{subCommands}" ]
                }

            let usages =
                verbDesc.usages
                |> Seq.map (fun usage -> [ "or:"; usage ])

            Seq.concat [ defaultUsages; usages ] |> Seq.toList

        let writeUsages verbChainName verbDesc =
            let tableOptions =
                cleanTableOptions
                |> withColumnAlignment ColumnTextAlignment.Right 0

            getUsagesTable verbChainName verbDesc
            |> writeTable tableOptions

            writeln ""

        let getMultiValuedHelp multiValued =
            match multiValued with
            | None -> ""
            | Some ({ minCount = None; maxCount = None }) -> " ..."
            | Some ({ minCount = Some minCount
                      maxCount = None }) -> $" %d{minCount}..."
            | Some ({ minCount = None
                      maxCount = Some maxCount }) -> $" ...%d{maxCount}"
            | Some ({ minCount = Some minCount
                      maxCount = Some maxCount }) -> $" %d{minCount}...%d{maxCount}"

        let getDefaultTextHelp defaultText =
            match defaultText with
            | Some text -> $" (Default: %s{text})"
            | None -> ""

        let getPositionalArgumentsTable arguments =
            let collector =
                function
                | CommandLinePositional (desc, argDesc, posDesc) -> [ desc, argDesc, posDesc ]
                | _ -> []

            let byPriority (_, _, posDesc) = posDesc.priority

            arguments
            |> Seq.collect collector
            |> Seq.sortBy byPriority
            |> Seq.map
                (fun (desc, argDesc, posDesc) ->
                    let key = argDesc.key
                    let multiplicity = getMultiValuedHelp posDesc.multiValued
                    let summary = getSummaryOf desc
                    let default' = getDefaultTextHelp posDesc.defaultText

                    [ $"%s{key}%s{multiplicity}"
                      $"%s{summary}%s{default'}" ])
            |> Seq.toList

        let writePositionalArguments prefix arguments =
            let tableOptions =
                { cleanTableOptions with
                      columnSeparator = "   "
                      columnSeparatorStart = "  " }
                |> withColumnMinWidth 14 0

            let table = getPositionalArgumentsTable arguments

            if table <> [] then
                writeln $"%s{prefix}Arguments:"

            writeTable tableOptions table

            if table <> [] then writeln ""

        let getOptionArgumentsTable arguments =
            let getForms longForms shortForms =
                let shortForms =
                    Seq.map (fun (c: char) -> $"-%c{c}") shortForms

                let longForms =
                    Seq.map (fun (s: string) -> $"--%s{s}") longForms

                Seq.concat [ shortForms; longForms ]
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

            let collector =
                function
                | CommandLineOption (desc, argDesc, optDesc) -> [ CommandLineOption(desc, argDesc, optDesc) ]
                | CommandLineFlag (desc, argDesc, flagDesc) -> [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | _ -> []

            arguments
            |> Seq.collect collector
            |> Seq.map getFormatted
            |> Seq.map (fun (forms, summary) -> [ forms; summary ])
            |> Seq.toList

        let writeOptionArguments prefix arguments =
            let tableOptions =
                { cleanTableOptions with
                      columnSeparator = "   "
                      columnSeparatorStart = "  " }
                |> withColumnMinWidth 14 0

            let table = getOptionArgumentsTable arguments

            if table <> [] then
                writeln $"%s{prefix}Options:"

            writeTable tableOptions table

            if table <> [] then writeln ""

        let getSubcommandsTable arguments =
            let collector =
                function
                | CommandLineVerb (desc, verbDesc) -> [ desc, verbDesc ]
                | _ -> []

            arguments
            |> Seq.collect collector
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

        let verbChainName path =
            path
            |> Seq.collect
                (function
                | CommandLineVerb (_, v) -> [ v.verb ]
                | _ -> [])
            |> Seq.rev
            |> String.concat " "

        let rec loop focused argPath =
            match argPath with
            | CommandLineVerb (desc, verbDesc) :: tail ->
                let verbChainName = verbChainName argPath

                if focused then
                    writeFocusedDesc desc
                    writeUsages verbChainName verbDesc
                    writePositionalArguments "" verbDesc.arguments
                    writeOptionArguments "" verbDesc.arguments
                else
                    writeParentDesc desc
                    writePositionalArguments $"%s{verbChainName} " verbDesc.arguments
                    writeOptionArguments $"%s{verbChainName} " verbDesc.arguments

                if focused then
                    writeCommandsArguments verbDesc.arguments
                    writeExamples verbDesc.examples

                loop false tail

            | _ -> ()

        writeErrors data.errors
        loop true data.argPath

    let buildPlan metadata tokens : BuildPlanResult =
        let searchArgument localOnly notFoundMsg multipleFoundMsg getMatches argPath =
            let findVerbCandidates arguments =
                arguments |> Seq.collect getMatches |> Seq.toList

            let rec loop argPath =
                match argPath with
                | [] -> []
                | CommandLineVerb (desc, verbDesc) :: argPath' ->
                    match findVerbCandidates verbDesc.arguments with
                    | [] -> if localOnly then [] else loop argPath'
                    | candidates -> candidates
                | other :: _ -> failwith $"Unexpected element %A{other}"

            loop argPath
            |> function
                | [] -> Error [ notFoundMsg ]
                | candidate :: [] -> Ok candidate
                | _ -> Error [ multipleFoundMsg ]

        let searchShortForm char =
            let containsForm = List.contains char

            searchArgument
                false
                $"Short option '-%c{char}' not found"
                $"Multiple arguments use short option '-%c{char}'"
                (function
                | CommandLineFlag (desc, argDesc, flagDesc) when containsForm flagDesc.shortForms ->
                    [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | CommandLineOption (desc, argDesc, optDesc) when containsForm optDesc.shortForms ->
                    [ CommandLineOption(desc, argDesc, optDesc) ]
                | _ -> [])

        let searchLongForm str =
            let containsForm = List.contains str

            searchArgument
                false
                $"Long option '--%s{str}' not found"
                $"Multiple arguments use long option '--%s{str}'"
                (function
                | CommandLineFlag (desc, argDesc, flagDesc) when containsForm flagDesc.longForms ->
                    [ CommandLineFlag(desc, argDesc, flagDesc) ]
                | CommandLineOption (desc, argDesc, optDesc) when containsForm optDesc.longForms ->
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
            let rec loopArgs arguments (valueMap: Map<string, obj>) =
                match arguments with
                | [] -> valueMap

                | CommandLineFlag (_, argDesc, _) :: tail ->
                    valueMap
                    |> Map.update
                        argDesc.key
                        (function
                        | Some v -> Some v
                        | None -> Some false)
                    |> loopArgs tail

                | CommandLineOption (_, argDesc, optDesc) :: tail ->
                    match optDesc.defaultValue with
                    | Some value ->
                        valueMap
                        |> Map.update
                            argDesc.key
                            (function
                            | Some v -> Some v
                            | None -> Some value)
                    | None -> valueMap
                    |> loopArgs tail

                | CommandLinePositional (_, argDesc, posDesc) :: tail ->
                    match posDesc.defaultValue with
                    | Some value ->
                        valueMap
                        |> Map.update
                            argDesc.key
                            (function
                            | Some v -> Some v
                            | None -> Some value)
                    | None -> valueMap
                    |> loopArgs tail

                | _ :: tail -> loopArgs tail valueMap

            let rec loopPath argPath valueMap =
                match argPath with
                | [] -> valueMap
                | CommandLineVerb (desc, verbDesc) :: tail ->
                    valueMap
                    |> loopArgs verbDesc.arguments
                    |> loopPath tail
                | other :: _ -> failwith $"Unexpected element: %A{other}"

            loopPath argPath valueMap

        let checkMultiValued prefix multiValued (value: obj) =
            match multiValued with
            | None -> []
            | Some constraints ->
                match value with
                | :? (list<obj>) as values ->
                    [ let count = List.length values

                      match constraints.minCount with
                      | Some minCount when count < minCount ->
                          yield
                              $"%s{prefix} was expected to have at least %i{minCount} elements, but %i{count} were found"
                      | _ -> ()

                      match constraints.maxCount with
                      | Some maxCount when count < maxCount ->
                          yield
                              $"%s{prefix} was expected to have at most %i{maxCount} elements, but %i{count} were found"
                      | _ -> () ]
                | other -> failwith $"Unexpected element %A{other}"

        let searchInvalidConstraints argPath (valueMap: Map<string, obj>) =
            let rec loopArgs arguments errors =
                match arguments with
                | [] -> errors

                | CommandLineOption (_, argDesc, optDesc) :: tail ->
                    match argDesc.isRequired with
                    | true ->
                        let form = getFormOfOption argDesc optDesc

                        match valueMap |> Map.tryFind argDesc.key with
                        | Some value ->
                            value
                            |> checkMultiValued $"Option %s{form}" optDesc.multiValued
                            |> (fun es -> es @ errors)
                        | None -> $"Missing required option %s{form}" :: errors
                        |> loopArgs tail
                    | false -> loopArgs tail errors

                | CommandLinePositional (_, argDesc, posDesc) :: tail ->
                    match argDesc.isRequired with
                    | true ->
                        match valueMap |> Map.tryFind argDesc.key with
                        | Some value ->
                            value
                            |> checkMultiValued $"Argument %s{argDesc.key}" posDesc.multiValued
                            |> (fun es -> es @ errors)
                        | None ->
                            $"Missing required option %s{argDesc.key}"
                            :: errors
                        |> loopArgs tail
                    | false -> loopArgs tail errors

                | _ :: tail -> loopArgs tail errors

            let rec loopPath argPath errors =
                match argPath with
                | [] -> List.rev errors
                | CommandLineVerb (desc, verbDesc) :: tail ->
                    errors
                    |> loopArgs verbDesc.arguments
                    |> loopPath tail
                | other :: _ -> failwith $"Unexpected element: %A{other}"

            loopPath argPath []

        let rec loop (state: BuildPlanState) =
            let asError errors valueMap =
                Error
                    { errors = errors
                      argPath = state.argPath
                      valueMap = valueMap }

            let asOk execute valueMap =
                Ok
                    { execute = execute
                      argPath = state.argPath
                      metadata = metadata
                      valueMap = valueMap }

            let nonExecutableError verbDesc =
                let subcommands =
                    getSubCommandVerbs verbDesc.arguments
                    |> String.concat ", "

                let error =
                    $"Command %s{verbDesc.verb} is not executable. "
                    + $"Try one of the sub-commands: %s{subcommands}"

                asError [ error ] state.valueMap

            let asExecutableCommand verbDesc =
                let valueMap =
                    fillDefaultValues state.argPath state.valueMap

                match searchInvalidConstraints state.argPath valueMap with
                | [] -> // When all required options are fulfilled
                    asOk verbDesc valueMap
                | errors -> // When some required options are missing
                    asError errors valueMap

            match state.tokens with
            | [] -> // When there are no more tokens to proecss
                match state.errors with
                | [] -> // When there are no errors from looking up for tokens in metadata
                    match state.argPath with
                    | CommandLineVerb (_, verbDesc) :: _ ->
                        match verbDesc.execute with
                        | None -> // When selected verb is not executable
                            nonExecutableError verbDesc
                        | Some execute -> asExecutableCommand execute

                    | other :: _ -> failwith $"Unexpected element %A{other}"
                    | [] -> failwith $"Unexpected empty list"

                | errors -> // When parsing errors
                    asError (List.rev errors) state.valueMap

            | token :: tokens' ->
                let withErrors errors =
                    { state with
                          errors = errors @ state.errors
                          tokens = tokens' }

                let withValuesAndTokens valueMap tokens' =
                    { state with
                          valueMap = valueMap
                          tokens = tokens' }

                let withValues valueMap = withValuesAndTokens valueMap tokens'

                let ofFlagOrOptionResult result =
                    match result with
                    | Error errors -> withErrors errors

                    | Ok (CommandLineFlag (desc, argDesc, flagDesc)) ->
                        match Map.tryFind argDesc.key state.valueMap with
                        | Some value ->
                            let form = getFormOfFlag argDesc flagDesc

                            withErrors [ $"Flag '%s{form}' was found multiple times" ]
                        | None -> withValues (state.valueMap |> Map.add argDesc.key true)

                    | Ok (CommandLineOption (desc, argDesc, optDesc)) ->
                        let readOptionValue () =
                            let rec loop tokens remaining inputs =
                                match tokens, remaining with
                                | _, n when n < 0 ->
                                    let form = getFormOfOption argDesc optDesc
                                    failwith $"Token count cannot be negative in {form}"
                                | tokens', 0 ->
                                    optDesc.reader.read (List.rev inputs)
                                    |> Result.map (fun value -> value, tokens')
                                    |> Result.mapError List.singleton
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
                            | Error errors -> withErrors errors
                            | Ok (value, tokens') ->
                                let value' = accumFn value
                                withValuesAndTokens (state.valueMap |> Map.add argDesc.key value') tokens'

                        match Map.tryFind argDesc.key state.valueMap, optDesc.multiValued with
                        | Some _, None -> // When existing value and single-valued option
                            let form = getFormOfOption argDesc optDesc
                            withErrors [ $"Option '%s{form}' was found multiple times" ]
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

    let executePlan (plan: BuildPlanSuccess) =
        async {
            let context: ExecuteVerbContext =
                { services = serviceProvider
                  showHelp =
                      fun errors ->
                          showHelp
                              { errors = errors
                                showExamples = false
                                metadata = plan.metadata
                                argPath = plan.argPath }
                  valueMap = plan.valueMap }

            try
                return! plan.execute context
            with
            | exn ->
                writelnC red $"%O{exn}"
                return { exitCode = 1 }
        }

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

                let getBoolKey optKey valueMap =
                    optKey
                    |> Option.map (fun key -> valueMap |> Metadata.tryGetOr false key)
                    |> Option.defaultValue false

                match executionPlan with
                | Error error ->
                    let examplesFlag =
                        getBoolKey metadata.examplesFlagKey error.valueMap

                    showHelp
                        { errors = error.errors
                          showExamples = examplesFlag
                          metadata = metadata
                          argPath = error.argPath }

                    return { exitCode = 1 }
                | Ok plan ->
                    let versionFlag =
                        getBoolKey metadata.versionFlagKey plan.valueMap

                    if versionFlag then
                        writeln metadata.version
                        return { exitCode = 0 }
                    else
                        let helpFlag =
                            getBoolKey metadata.helpFlagKey plan.valueMap

                        if helpFlag then
                            let examplesFlag =
                                getBoolKey metadata.examplesFlagKey plan.valueMap

                            showHelp
                                { errors = []
                                  showExamples = examplesFlag
                                  metadata = metadata
                                  argPath = plan.argPath }

                            return { exitCode = 0 }
                        else
                            return! executePlan plan
        }

    interface ICommandLineRuntime with
        member this.Execute context = execute context
