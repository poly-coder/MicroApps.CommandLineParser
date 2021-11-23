namespace MicroApps.CommandLineParser

open Preamble

type ICommandLineMetadataProvider =
    abstract GetMetadata: unit -> Async<CommandLineMetadata>

type StaticCommandLineMetadataProvider(metadata) =
    interface ICommandLineMetadataProvider with
        member this.GetMetadata() = async.Return metadata

open Console
open System

module CommandLineMetadataRuntime =
    type ShowHelpOptions =
        { errors: string list
          showDetails: bool
          metadata: CommandLineMetadata
          verbStack: VerbDesc list }

    type BuildPlanState =
        { errors: string list
          verbStack: VerbDesc list
          valueMap: Map<string, obj>
          tokens: ArgumentToken list }

    type BuildPlanError =
        { errors: string list
          verbStack: VerbDesc list
          valueMap: Map<string, obj> }

    type BuildPlanSuccess =
        { execute: ExecuteVerbFunc
          verbStack: VerbDesc list
          valueMap: Map<string, obj> }

    type BuildPlanResult = Result<BuildPlanSuccess, BuildPlanError>

    type FormSearchResult =
        | FlagFormFound of FlagDesc
        | OptionFormFound of OptionDesc

    let getFlags group = group.flags
    let getOptions group = group.options
    let getArguments group = group.arguments
    let getVerbs group = group.verbs
    let getGroups verb = verb.groups
    let getFlagKey (flag: FlagDesc) = flag.key
    let getOptionKey (option: OptionDesc) = option.key
    let getArgumentKey (argument: ArgumentDesc) = argument.key
    let getVerbKey (verb: VerbDesc) = verb.key

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

    let getSubVerbList verbDesc =
        verbDesc.groups
        |> Seq.collect (fun group -> group.verbs)
        |> Seq.map (fun verb -> verb.key)
        |> Seq.toList

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
          yield! longForms |> Seq.map (sprintf "--%s") ]
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

            yield!
                groupDesc.usages
                |> Seq.map (sprintf "%s %s" prefix)
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
                    |> withColumnMinWidth 14 1
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
                    |> withColumnMinWidth 14 1
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

    let writeExamples showDetails prefix groupDesc =
        match groupDesc.examples with
        | [] -> ()
        | examples when showDetails ->
            writeln $"%s{prefix}Examples:"

            for example in examples do
                writeln example
                writeln ""
        | _ -> ()

    let writeGroups showDetails focused verbChain verbDesc =
        verbDesc.groups
        |> Seq.iteri
            (fun index groupDesc ->
                let prefix =
                    if index = 0 then
                        ""
                    else
                        (getNameOf groupDesc.desc) + " "

                if groupDesc.showSummary then
                    if focused then
                        writeFocusedDesc groupDesc.desc
                    else
                        writeParentDesc groupDesc.desc

                writeUsages verbChain groupDesc
                writeArguments prefix groupDesc
                writeFlags prefix groupDesc
                writeOptions prefix groupDesc

                if focused then
                    writeVerbs prefix groupDesc
                    writeExamples showDetails prefix groupDesc

                )

    let showHelp (data: ShowHelpOptions) =
        let rec helpLoop focused verbStack =
            let verbChain = getVerbChain verbStack

            match verbStack with
            | verbDesc :: tail ->
                writeGroups data.showDetails focused verbChain verbDesc
                helpLoop false tail
            | _ -> ()

        writeErrors data.errors
        helpLoop true data.verbStack

    let searchOnStack localOnly notFoundMsg multipleFoundMsg getCollection verbStack =
        let findCandidates verbDesc =
            verbDesc.groups
            |> Seq.collect getCollection
            |> Seq.toList

        let rec loop verbStack =
            match verbStack with
            | [] -> []
            | verbDesc :: verbStack' ->
                match findCandidates verbDesc with
                | [] ->
                    if localOnly then
                        []
                    else
                        loop verbStack'
                | candidates -> candidates

        loop verbStack
        |> function
            | [] -> Error [ notFoundMsg ]
            | candidate :: [] -> Ok candidate
            | _ -> Error [ multipleFoundMsg ]

    let searchShortForm char =
        let containsForm = List.contains char

        let getCollection group =
            seq {
                yield!
                    group.flags
                    |> Seq.filter (fun flag -> containsForm flag.shortForms)
                    |> Seq.map FlagFormFound

                yield!
                    group.options
                    |> Seq.filter (fun flag -> containsForm flag.shortForms)
                    |> Seq.map OptionFormFound
            }

        searchOnStack
            false
            $"Short option '-%c{char}' not found"
            $"Multiple arguments use short option '-%c{char}'"
            getCollection

    let searchLongForm str =
        let containsForm = List.contains str

        let getCollection group =
            seq {
                yield!
                    group.flags
                    |> Seq.filter (fun flag -> containsForm flag.longForms)
                    |> Seq.map FlagFormFound

                yield!
                    group.options
                    |> Seq.filter (fun flag -> containsForm flag.longForms)
                    |> Seq.map OptionFormFound
            }

        searchOnStack
            false
            $"Long option '--%s{str}' not found"
            $"Multiple arguments use long option '--%s{str}'"
            getCollection

    let searchVerb str =
        let getCollection group =
            group.verbs
            |> Seq.filter (fun verb -> verb.key = str)

        searchOnStack true $"Command '%s{str}' not found" $"Multiple commands use verb '%s{str}'" getCollection

    let fillDefaultValues verbStack valueMap =
        let loopItems getCollection getKey getDefaultValue verbDesc valueMap =
            verbDesc.groups
            |> Seq.collect getCollection
            |> Seq.fold
                (fun acc item ->
                    match getDefaultValue item with
                    | None -> acc
                    | Some value ->
                        acc
                        |> Map.update
                            (getKey item)
                            (function
                            | Some v -> Some v
                            | None -> Some value))
                valueMap

        let loopFlags =
            let someFalse _ = Some(false :> obj)
            loopItems getFlags getFlagKey someFalse

        let loopOptions =
            let getDefault (o: OptionDesc) = o.defaultValue |> Option.map fst
            loopItems getOptions getOptionKey getDefault

        let loopArguments =
            let getDefault (a: ArgumentDesc) = a.defaultValue |> Option.map fst
            loopItems getArguments getArgumentKey getDefault

        let loopVerb verbDesc (valueMap: Map<string, obj>) =
            valueMap
            |> loopFlags verbDesc
            |> loopOptions verbDesc
            |> loopArguments verbDesc

        let rec loopPath verbStack valueMap =
            match verbStack with
            | [] -> valueMap
            | verbDesc :: tail -> valueMap |> loopVerb verbDesc |> loopPath tail

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
                      yield $"%s{prefix} was expected to have at least %i{minCount} elements, but %i{count} were found"
                  | _ -> ()


                  match constraints.maxCount with
                  | Some maxCount when count < maxCount ->
                      yield $"%s{prefix} was expected to have at most %i{maxCount} elements, but %i{count} were found"
                  | _ -> () ]
            | other -> failwith $"Unexpected element %A{other}"

    let searchInvalidConstraints verbStack (valueMap: Map<string, obj>) =
        let loopItems getCollection getErrors verbDesc errors =
            verbDesc.groups
            |> Seq.collect getCollection
            |> Seq.fold
                (fun acc item ->
                    let newErrors = getErrors item
                    newErrors @ acc)
                errors

        let loopOptions =
            let getErrors (optDesc: OptionDesc) =
                match valueMap |> Map.tryFind optDesc.key with
                | Some value ->
                    let form = getOptionForm optDesc
                    checkMultiValued $"Option %s{form}" optDesc.multiValued value
                | None ->
                    if optDesc.isRequired then
                        let form = getOptionForm optDesc
                        [ $"Missing required option %s{form}" ]
                    else
                        []

            loopItems getOptions getErrors

        let loopArguments =
            let getErrors (argDesc: ArgumentDesc) =
                match valueMap |> Map.tryFind argDesc.key with
                | Some value ->
                    let form = argDesc.key
                    checkMultiValued $"Argument %s{form}" argDesc.multiValued value
                | None ->
                    if argDesc.isRequired then
                        let form = argDesc.key
                        [ $"Missing required argument %s{form}" ]
                    else
                        []

            loopItems getArguments getErrors

        let rec loopVerb verbDesc =
            loopOptions verbDesc >> loopArguments verbDesc

        let rec loopPath verbStack errors =
            match verbStack with
            | [] -> List.rev errors
            | verbDesc :: tail -> errors |> loopVerb verbDesc |> loopPath tail

        loopPath verbStack []

    let buildError errors verbStack valueMap =
        Error
            { errors = errors
              verbStack = verbStack
              valueMap = valueMap }

    let buildSuccess execute verbStack valueMap =
        Ok
            { execute = execute
              verbStack = verbStack
              valueMap = valueMap }

    let checkExecutingCommand (verbStack: VerbDesc list) valueMap =
        match verbStack with
        | verbDesc :: _ ->
            match verbDesc.execute with
            | None -> // When selected verb is not executable
                let subcommands =
                    getSubVerbList verbDesc |> String.concat ", "

                let error =
                    $"Command %s{verbDesc.key} is not executable. "
                    + $"Try one of the sub-commands: %s{subcommands}"

                buildError [ error ] verbStack valueMap

            | Some execute ->
                let valueMap = fillDefaultValues verbStack valueMap

                match searchInvalidConstraints verbStack valueMap with
                | [] -> // When all required options are fulfilled
                    buildSuccess execute verbStack valueMap
                | errors -> // When some required options are missing
                    buildError errors verbStack valueMap

        | [] -> failwith $"Unexpected empty command list"

open CommandLineMetadataRuntime

type CommandLineMetadataRuntime
    (
        metadataProvider: ICommandLineMetadataProvider,
        tokenizer: ICommandLineTokenizer,
        serviceProvider: IServiceProvider
    ) =

    let buildPlan metadata tokens : BuildPlanResult =

        let rec buildLoop (state: BuildPlanState) =
            match state.tokens with
            | [] -> // When there are no more tokens to proecss
                match state.errors with
                | [] -> // When there are no errors from looking up for tokens in metadata
                    checkExecutingCommand state.verbStack state.valueMap

                | errors -> // When parsing errors
                    buildError (List.rev errors) state.verbStack state.valueMap

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

                    | Ok (FlagFormFound flagDesc) ->
                        match Map.tryFind flagDesc.key state.valueMap with
                        | Some _ ->
                            let form = getFlagForm flagDesc
                            withErrors [ $"Flag '%s{form}' was found multiple times" ]
                        | None -> withValues (state.valueMap |> Map.add flagDesc.key true)

                    | Ok (OptionFormFound optDesc) ->
                        let readOptionValue () =
                            let rec readLoop tokens remaining inputs =
                                match tokens, remaining with
                                | _, n when n < 0 ->
                                    let form = getOptionForm optDesc
                                    failwith $"Token count cannot be negative reading {form} values"

                                | tokens', 0 ->
                                    optDesc.reader.read (List.rev inputs)
                                    |> Result.map (fun value -> value, tokens')
                                    |> Result.mapError List.singleton

                                | [], n ->
                                    let form = getOptionForm optDesc
                                    Error [ $"Expecting %i{n} additional parameters but no more arguments were given, reading {form} values" ]

                                | ValueToken str :: tokens', n -> readLoop tokens' (n - 1) (str :: inputs)

                                | ShortFormToken char :: _, n ->
                                    let form = getOptionForm optDesc
                                    Error [ $"Expecting %i{n} additional parameters but found '%c{char}', reading {form} values" ]

                                | LongFormToken str :: _, n ->
                                    let form = getOptionForm optDesc
                                    Error [ $"Expecting %i{n} additional parameters but found '%s{str}', reading {form} values" ]

                            readLoop tokens' optDesc.reader.tokenCount []

                        let accumulateValue accumFn =
                            match readOptionValue () with
                            | Error errors -> withErrors errors

                            | Ok (value, tokens') ->
                                let value' = accumFn value
                                withValuesAndTokens (state.valueMap |> Map.add optDesc.key value') tokens'

                        match Map.tryFind optDesc.key state.valueMap, optDesc.multiValued with
                        | Some _, None -> // When existing value and single-valued option
                            let form = getOptionForm optDesc
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

                match token with
                | ShortFormToken char ->
                    searchShortForm char state.verbStack
                    |> ofFlagOrOptionResult
                    |> buildLoop

                | LongFormToken str ->
                    searchLongForm str state.verbStack
                    |> ofFlagOrOptionResult
                    |> buildLoop

                | ValueToken str ->
                    match searchVerb str state.verbStack with
                    | Error errors ->
                        { state with
                              errors = errors @ state.errors
                              tokens = tokens' }
                        |> buildLoop

                    | Ok verbDesc ->
                        { state with
                              verbStack = verbDesc :: state.verbStack
                              tokens = tokens' }
                        |> buildLoop

        buildLoop
            { errors = []
              verbStack = [ metadata.verb ]
              valueMap = Map.empty
              tokens = tokens }

    let executePlan metadata (plan: BuildPlanSuccess) =
        async {
            let context: ExecuteVerbContext =
                { services = serviceProvider
                  showHelp =
                      fun errors ->
                          showHelp
                              { errors = errors
                                showDetails = false
                                metadata = metadata
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
                      showDetails = false
                      metadata = metadata
                      verbStack = [ metadata.verb ] }

                return { exitCode = 1 }
            | Ok tokens ->
                let executionPlan = buildPlan metadata tokens

                let getBoolKey optKey valueMap =
                    optKey
                    |> Option.map (fun key -> valueMap |> Metadata.tryGetOr false key)
                    |> Option.defaultValue false

                match executionPlan with
                | Error error ->
                    let detailedHelpFlag =
                        getBoolKey metadata.detailedHelpFlagKey error.valueMap

                    showHelp
                        { errors = error.errors
                          showDetails = detailedHelpFlag
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
                        let detailedHelpFlag =
                            getBoolKey metadata.detailedHelpFlagKey plan.valueMap

                        let helpFlag =
                            detailedHelpFlag
                            || getBoolKey metadata.helpFlagKey plan.valueMap

                        if helpFlag then

                            showHelp
                                { errors = []
                                  showDetails = detailedHelpFlag
                                  metadata = metadata
                                  verbStack = plan.verbStack }

                            return { exitCode = 0 }
                        else
                            return! executePlan metadata plan
        }

    interface ICommandLineRuntime with
        member this.Execute context = execute context
