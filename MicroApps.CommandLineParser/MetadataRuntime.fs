namespace MicroApps.CommandLineParser

open Preamble

type ICommandLineMetadataProvider =
    abstract GetMetadata: unit -> Async<CommandLineMetadata>

type StaticCommandLineMetadataProvider(metadata) =
    interface ICommandLineMetadataProvider with
        member this.GetMetadata() = async.Return metadata

open Console
open System

type internal ShowHelpOptions =
    { errors: string list
      showDetails: bool
      metadata: CommandLineMetadata
      verbStack: VerbDesc list }

type internal BuildPlanState =
    { errors: string list
      verbStack: VerbDesc list
      valueMap: Map<string, obj>
      tokens: ArgumentToken list }

type internal BuildPlanError =
    { errors: string list
      verbStack: VerbDesc list
      valueMap: Map<string, obj> }

type internal BuildPlanSuccess =
    { execute: ExecuteVerbFunc
      metadata: CommandLineMetadata
      verbStack: VerbDesc list
      valueMap: Map<string, obj> }

type internal BuildPlanResult = Result<BuildPlanSuccess, BuildPlanError>

type CommandLineMetadataRuntime
    (
        metadataProvider: ICommandLineMetadataProvider,
        tokenizer: ICommandLineTokenizer,
        serviceProvider: IServiceProvider
    ) =

    let getNameOfOr desc =
        if isNotNullOrWS desc.name then
            Some desc.name
        else
            None

    let getSummaryOfOr desc =
        if isNotNullOrWS desc.summary then
            Some desc.summary
        else
            getNameOfOr desc

    let getDescriptionOfOr desc =
        if isNotNullOrWS desc.description then
            Some desc.description
        else
            getSummaryOfOr desc

    let getNameOf = getNameOfOr >> Option.defaultValue ""
    let getSummaryOf = getSummaryOfOr >> Option.defaultValue ""

    let getDescriptionOf =
        getDescriptionOfOr >> Option.defaultValue ""

    let getFlagForm (desc: FlagDesc) =
        match desc.longForms with
        | first :: _ -> $"--%s{first}"
        | [] ->
            match desc.shortForms with
            | first :: _ -> $"-%c{first}"
            | [] -> desc.key

    let getOptionForm (desc: OptionDesc) =
        match desc.longForms with
        | first :: _ -> $"--%s{first}"
        | [] ->
            match desc.shortForms with
            | first :: _ -> $"-%c{first}"
            | [] -> desc.key

    let showHelp (data: ShowHelpOptions) =
        let writeErrors errors =
            match errors with
            | [] -> ()
            | errors ->
                writelnC red "Errors:"

                for error in errors do
                    writelnC red $"  * %s{error}"

        let writeFocusedDesc (desc: ElementDesc) =
            if isNotNullOrWS desc.name then
                writelnC white $"%s{desc.name}"
                writeln ""

            if isNotNullOrWS desc.description then
                writelnC gray desc.description
                writeln ""
            elif isNotNullOrWS desc.summary then
                writelnC gray desc.summary
                writeln ""

        let writeParentDesc (desc: ElementDesc) =
            if isNotNullOrWS desc.name then
                writelnC white $"%s{desc.name}"
                writeln ""

        let (|HasNoFlags|HasFlags|) groupDesc =
            match groupDesc.flags with
            | [] -> HasNoFlags
            | _ -> HasFlags

        let isRequiredOption (o: OptionDesc) = o.isRequired

        let (|HasNoOptions|HasRequiredOptions|HasOptions|) groupDesc =
            match groupDesc.options with
            | [] -> HasNoOptions
            | ls when ls |> List.exists isRequiredOption -> HasRequiredOptions
            | _ -> HasOptions

        let isRequiredArgument (o: ArgumentDesc) = o.isRequired

        let (|HasNoArguments|HasRequiredArguments|HasArguments|) groupDesc =
            match groupDesc.arguments with
            | [] -> HasNoArguments
            | ls when ls |> List.exists isRequiredArgument -> HasRequiredArguments
            | _ -> HasArguments

        let (|HasNoVerbs|HasVerbs|) groupDesc =
            match groupDesc.verbs with
            | [] -> HasNoVerbs
            | _ -> HasVerbs

        let getMultiValuedHelp multiValued =
            // No multi valued : ""
            // Multivalued: " ..."
            // MultiValued [): " min..."
            // MultiValued (]: " ...max"
            // MultiValued []: " min...max"
            match multiValued with
            | None -> ""
            | Some ({ minCount = None; maxCount = None }) -> " ..."
            | Some ({ minCount = Some minCount
                      maxCount = None }) -> $" %d{minCount}..."
            | Some ({ minCount = None
                      maxCount = Some maxCount }) -> $" ...%d{maxCount}"
            | Some ({ minCount = Some minCount
                      maxCount = Some maxCount }) -> $" %d{minCount}...%d{maxCount}"

        let getDefaultTextHelp =
            Option.map snd
            >> Option.map (sprintf "=%s")
            >> Option.defaultValue ""

        let getTriggers shortForms longForms =
            [ yield! shortForms |> Seq.map (sprintf "-%c")
              yield! longForms |> Seq.map (sprintf "-%s") ]
            |> String.concat "|"

        let getVerbChain (verbStack: VerbDesc list) =
            verbStack
            |> Seq.map (fun v -> v.key)
            |> Seq.rev
            |> String.concat " "

        let writeUsages (prefix: string) groupDesc =
            // usage: prefix first usage, which could be a default generated one if indicated
            //    or: prefix second usage
            //    or: ...
            seq {
                if groupDesc.showDefaultUsage then
                    // prefix [FLAGS] [OPTIONS] [ARGUMENTS] <COMMAND>
                    yield
                        [ prefix

                          match groupDesc with
                          | HasNoFlags -> ""
                          | HasFlags -> " [FLAGS]"

                          match groupDesc with
                          | HasNoOptions -> ""
                          | HasRequiredOptions -> " <OPTIONS>"
                          | HasOptions -> " [OPTIONS]"

                          match groupDesc with
                          | HasNoArguments -> ""
                          | HasRequiredArguments -> " <ARGUMENTS>"
                          | HasArguments -> " [ARGUMENTS]"

                          match groupDesc with
                          | HasNoVerbs -> ""
                          | HasVerbs -> " <COMMAND>" ]
                        |> String.concat ""

                yield! groupDesc.usages
            }
            |> Seq.mapi
                (fun index usage ->
                    if index = 0 then
                        [ "usage"; usage ]
                    else
                        [ "or"; usage ])
            |> Seq.toList
            |> fun table ->
                let tableOptions =
                    { cleanTableOptions with
                          separator = ": " }
                    |> withColumnAlignment ColumnTextAlignment.Right 0

                writeTable tableOptions table
                writeln ""

        let writeArguments prefix groupDesc =
            groupDesc.arguments
            |> Seq.map
                (fun arg ->
                    // * arg-name | <type(s)> ... =<default> | Summary
                    let isRequired = if arg.isRequired then $"* " else ""
                    let argName = arg.key
                    let summary = getSummaryOf arg.desc

                    let typeConfig =
                        [ $"<%s{arg.reader.typeName}>"
                          getMultiValuedHelp arg.multiValued
                          getDefaultTextHelp arg.defaultValue ]
                        |> String.concat " "

                    [ isRequired
                      argName
                      typeConfig
                      summary ])
            |> Seq.toList
            |> function
                | [] -> ()
                | table ->
                    let tableOptions =
                        { cleanTableOptions with
                              separator = "   "
                              separatorStart = "  " }
                        |> withColumnMinWidth 14 0
                        |> withColumnSeparator "" 0

                    writeln $"%s{prefix}Arguments:"
                    writeTable tableOptions table
                    writeln ""

        let writeFlags prefix groupDesc =
            groupDesc.flags
            |> Seq.map
                (fun arg ->
                    // * -f|--flag | Summary
                    let triggers = getTriggers arg.shortForms arg.longForms
                    let summary = getSummaryOf arg.desc
                    [ triggers; summary ])
            |> Seq.toList
            |> function
                | [] -> ()
                | table ->
                    let tableOptions =
                        { cleanTableOptions with
                              separator = "   "
                              separatorStart = "  " }
                        |> withColumnMinWidth 14 0

                    writeln $"%s{prefix}Flags:"
                    writeTable tableOptions table
                    writeln ""

        let writeOptions prefix groupDesc =
            groupDesc.options
            |> Seq.map
                (fun arg ->
                    // * -o|--option | <type(s)> ... =<default> | Summary
                    let isRequired = if arg.isRequired then $"* " else ""
                    let triggers = getTriggers arg.shortForms arg.longForms
                    let summary = getSummaryOf arg.desc

                    let typeConfig =
                        [ $"<%s{arg.reader.typeName}>"
                          getMultiValuedHelp arg.multiValued
                          getDefaultTextHelp arg.defaultValue ]
                        |> String.concat " "

                    [ isRequired
                      triggers
                      typeConfig
                      summary ])
            |> Seq.toList
            |> function
                | [] -> ()
                | table ->
                    let tableOptions =
                        { cleanTableOptions with
                              separator = "   "
                              separatorStart = "  " }
                        |> withColumnMinWidth 14 0
                        |> withColumnSeparator "" 0

                    writeln $"%s{prefix}Options:"
                    writeTable tableOptions table
                    writeln ""

        let writeVerbs prefix groupDesc =
            groupDesc.verbs
            |> Seq.map
                (fun arg ->
                    // command | Summary
                    let command = arg.key

                    let summary =
                        match arg.groups with
                        | [] -> "This verb has no functionality"
                        | head :: _ -> getSummaryOf head.desc

                    [ command; summary ])
            |> Seq.toList
            |> function
                | [] -> ()
                | table ->
                    let tableOptions =
                        { cleanTableOptions with
                              separator = "   "
                              separatorStart = "  " }
                        |> withColumnMinWidth 14 0

                    writeln $"%s{prefix}Commands:"
                    writeTable tableOptions table
                    writeln ""

        let writeExamples prefix groupDesc =
            match groupDesc.examples with
            | [] -> ()
            | examples when data.showDetails ->
                writeln $"%s{prefix}Examples:"

                for example in examples do
                    writeln example
                    writeln ""
            | _ -> ()

        let writeGroups focused verbChain verbDesc =
            verbDesc.groups
            |> Seq.iteri
                (fun index groupDesc ->
                    let prefix =
                        if index = 0 then
                            ""
                        else
                            (getNameOf groupDesc.desc) + " "

                    if focused then
                        writeFocusedDesc groupDesc.desc
                    else
                        writeParentDesc groupDesc.desc

                    writeUsages verbChain groupDesc
                    writeArguments verbChain groupDesc
                    writeFlags verbChain groupDesc
                    writeOptions verbChain groupDesc

                    if focused then
                        writeVerbs verbChain groupDesc
                        writeExamples verbChain groupDesc

                    )

        let rec helpLoop focused verbStack =
            let verbChain = getVerbChain verbStack

            match verbStack with
            | verbDesc :: tail ->
                writeGroups focused verbChain verbDesc
                helpLoop false tail
            | _ -> ()

        writeErrors data.errors
        helpLoop true data.verbStack

    let buildPlan metadata tokens : BuildPlanResult =
        let searchArgument localOnly notFoundMsg multipleFoundMsg getMatches verbStack =
            let findVerbCandidates arguments =
                arguments |> Seq.collect getMatches |> Seq.toList

            let rec loop verbStack =
                match verbStack with
                | [] -> []
                | CommandLineVerb (desc, verbDesc) :: verbStack' ->
                    match findVerbCandidates verbDesc.arguments with
                    | [] ->
                        if localOnly then
                            []
                        else
                            loop verbStack'
                    | candidates -> candidates
                | other :: _ -> failwith $"Unexpected element %A{other}"

            loop verbStack
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

        let fillDefaultValues verbStack valueMap =
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

            let rec loopPath verbStack valueMap =
                match verbStack with
                | [] -> valueMap
                | CommandLineVerb (desc, verbDesc) :: tail ->
                    valueMap
                    |> loopArgs verbDesc.arguments
                    |> loopPath tail
                | other :: _ -> failwith $"Unexpected element: %A{other}"

            loopPath verbStack valueMap

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

        let searchInvalidConstraints verbStack (valueMap: Map<string, obj>) =
            let rec loopArgs arguments errors =
                match arguments with
                | [] -> errors

                | CommandLineOption (_, argDesc, optDesc) :: tail ->
                    match argDesc.isRequired with
                    | true ->
                        let form = getOptionForm argDesc optDesc

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

            let rec loopPath verbStack errors =
                match verbStack with
                | [] -> List.rev errors
                | CommandLineVerb (desc, verbDesc) :: tail ->
                    errors
                    |> loopArgs verbDesc.arguments
                    |> loopPath tail
                | other :: _ -> failwith $"Unexpected element: %A{other}"

            loopPath verbStack []

        let rec loop (state: BuildPlanState) =
            let asError errors valueMap =
                Error
                    { errors = errors
                      verbStack = state.verbStack
                      valueMap = valueMap }

            let asOk execute valueMap =
                Ok
                    { execute = execute
                      verbStack = state.verbStack
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
                    fillDefaultValues state.verbStack state.valueMap

                match searchInvalidConstraints state.verbStack valueMap with
                | [] -> // When all required options are fulfilled
                    asOk verbDesc valueMap
                | errors -> // When some required options are missing
                    asError errors valueMap

            match state.tokens with
            | [] -> // When there are no more tokens to proecss
                match state.errors with
                | [] -> // When there are no errors from looking up for tokens in metadata
                    match state.verbStack with
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
                            let form = getFlagForm argDesc flagDesc

                            withErrors [ $"Flag '%s{form}' was found multiple times" ]
                        | None -> withValues (state.valueMap |> Map.add argDesc.key true)

                    | Ok (CommandLineOption (desc, argDesc, optDesc)) ->
                        let readOptionValue () =
                            let rec loop tokens remaining inputs =
                                match tokens, remaining with
                                | _, n when n < 0 ->
                                    let form = getOptionForm argDesc optDesc
                                    failwith $"Token count cannot be negative in {form}"
                                | tokens', 0 ->
                                    optDesc.reader.read (List.rev inputs)
                                    |> Result.map (fun value -> value, tokens')
                                    |> Result.mapError List.singleton
                                | [], n ->
                                    let form = getOptionForm argDesc optDesc
                                    Error [ $"Expecting %i{n} additional parameters but no more arguments were given, reading {form}" ]
                                | ValueToken str :: tokens', n -> loop tokens' (n - 1) (str :: inputs)
                                | ShortFormToken char :: _, n ->
                                    let form = getOptionForm argDesc optDesc
                                    Error [ $"Expecting %i{n} additional parameters but found '%c{char}', reading {form}" ]
                                | LongFormToken str :: _, n ->
                                    let form = getOptionForm argDesc optDesc
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
                            let form = getOptionForm argDesc optDesc
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
                    searchShortForm char state.verbStack
                    |> ofFlagOrOptionResult
                    |> loop
                | LongFormToken str ->
                    searchLongForm str state.verbStack
                    |> ofFlagOrOptionResult
                    |> loop
                | ValueToken str ->
                    match searchCommand str state.verbStack with
                    | Error errors ->
                        { state with
                              errors = errors @ state.errors
                              tokens = tokens' }
                        |> loop
                    | Ok (CommandLineVerb (desc, verbDesc)) ->
                        { state with
                              verbStack =
                                  (CommandLineVerb(desc, verbDesc))
                                  :: state.verbStack
                              tokens = tokens' }
                        |> loop
                    | Ok other -> failwith $"Unexpected element %A{other}"

        loop
            { errors = []
              verbStack = [ CommandLineVerb(metadata.desc, metadata.verb) ]
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
                                verbStack = plan.verbStack }
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
                      verbStack = [ CommandLineVerb(metadata.desc, metadata.verb) ] }

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
                          verbStack = error.verbStack }

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
                                  verbStack = plan.verbStack }

                            return { exitCode = 0 }
                        else
                            return! executePlan plan
        }

    interface ICommandLineRuntime with
        member this.Execute context = execute context
