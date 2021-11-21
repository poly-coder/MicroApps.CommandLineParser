namespace MicroApps.CommandLineParser

open System
open Preamble
open System.IO

type ValueReader =
    { tokenCount: int
      typeName: string
      read: string list -> Result<obj, string> }

type ElementDescription =
    { name: string
      summary: string
      description: string }

type MultiValue =
    { minCount: int option
      maxCount: int option }

type ExecuteVerbContext = { services: IServiceProvider }

type ExecuteVerbFunc = ExecuteVerbContext -> Async<CommandLineExecuteResult>

type ArgumentDescription = { key: string; isRequired: bool }

type CommandLineArgument =
    | CommandLineFlag of ElementDescription * ArgumentDescription * FlagDescription
    | CommandLineOption of ElementDescription * ArgumentDescription * OptionDescription
    | CommandLinePositional of ElementDescription * ArgumentDescription * PositionalDescription
    | CommandLineVerb of ElementDescription * VerbDescription
//| CommandLineGroup of ElementDescription * GroupDescription

and FlagDescription =
    { shortForms: char list
      longForms: string list }

and OptionDescription =
    { multiValued: MultiValue option
      shortForms: char list
      longForms: string list
      defaultText: string option
      defaultValue: obj option
      reader: ValueReader }

and PositionalDescription =
    { multiValued: MultiValue option
      priority: int
      defaultText: string option
      defaultValue: obj option
      reader: ValueReader }

and VerbDescription =
    { verb: string
      arguments: CommandLineArgument list
      isDefault: bool
      examples: string list
      usages: string list
      execute: ExecuteVerbFunc option }

//and GroupDescription = { arguments: CommandLineArgument list }

type CommandLineMetadata =
    { helpFlagKey: string option
      examplesFlagKey: string option
      versionFlagKey: string option
      desc: ElementDescription
      verb: VerbDescription }

module Metadata =
    // Value Readers

    let readOne tryRead values =
        match values with
        | [] -> Error "Too few values to read"
        | [ value ] -> tryRead value |> Result.map (fun v -> v :> obj)
        | _ -> Error "Too many values to read"

    let tryParseReader typeName tryParse : ValueReader =
        { tokenCount = 1
          typeName = $"%s{typeName}"
          read =
              readOne
                  (fun v ->
                      match tryParse v with
                      | true, v -> Ok v
                      | false, _ -> Error $"Invalid %s{typeName} value: %s{v}") }

    let stringReader: ValueReader =
        { tokenCount = 1
          typeName = "<string>"
          read = readOne Ok }

    let namedStringReader typeName =
        { stringReader with
              typeName = $"%s{typeName}" }

    let charReader = tryParseReader "char" Char.TryParse

    let int8Reader = tryParseReader "integer" SByte.TryParse
    let int16Reader = tryParseReader "integer" Int16.TryParse
    let int32Reader = tryParseReader "integer" Int32.TryParse
    let int64Reader = tryParseReader "integer" Int64.TryParse

    let uint8Reader = tryParseReader "integer" Byte.TryParse
    let uint16Reader = tryParseReader "integer" UInt16.TryParse
    let uint32Reader = tryParseReader "integer" UInt32.TryParse
    let uint64Reader = tryParseReader "integer" UInt64.TryParse

    let singleReader = tryParseReader "decimal" Single.TryParse
    let doubleReader = tryParseReader "decimal" Double.TryParse

    let decimalReader =
        tryParseReader "decimal" Decimal.TryParse

    let dateTimeReader =
        tryParseReader "date-time" DateTime.TryParse

    let dateTimeOffsetReader =
        tryParseReader "date-time" DateTimeOffset.TryParse

    let timeSpanReader =
        tryParseReader "time-span" TimeSpan.TryParse

    let existingFilePathReader =
        let tryParse text =
            if File.Exists(text) then
                true, text
            else
                false, ""

        tryParseReader "local-file" tryParse

    let existingDirectoryPathReader =
        let tryParse text =
            if Directory.Exists(text) then
                true, text
            else
                false, ""

        tryParseReader "local-file" tryParse

    let enumReader<'enum when 'enum: struct and 'enum: (new : unit -> 'enum) and 'enum :> System.Enum> typeName =
        tryParseReader typeName System.Enum.TryParse<'enum>

    // ElementDescription

    let elementDescription: ElementDescription =
        { name = ""
          summary = ""
          description = "" }

    let named name = { elementDescription with name = name }

    let summarized summary =
        { elementDescription with
              summary = summary }

    let withName name d = { d with name = name }
    let withSummary summary d = { d with summary = summary }
    let withDescription description d = { d with description = description }

    // MultiValue

    let multiValued =
        Some({ minCount = None; maxCount = None })

    let multiValuedMin min =
        Some({ minCount = Some min; maxCount = None })

    let multiValuedMax max =
        Some({ minCount = None; maxCount = Some max })

    let multiValuedBetween min max =
        Some(
            { minCount = Some min
              maxCount = Some max }
        )

    // ArgumentDescription

    let optional key = { key = key; isRequired = false }
    let required key = { key = key; isRequired = true }

    // FlagDescription

    let longFlagArg longForm : FlagDescription =
        { shortForms = []
          longForms = [ longForm ] }

    let shortFlagArg shortForm : FlagDescription =
        { shortForms = [ shortForm ]
          longForms = [] }

    let flagArg longForm shortForm : FlagDescription =
        { shortForms = [ shortForm ]
          longForms = [ longForm ] }

    let withLongFlag longForm f : FlagDescription =
        { f with
              longForms = f.longForms @ [ longForm ] }

    let withShortFlag shortForm f : FlagDescription =
        { f with
              shortForms = f.shortForms @ [ shortForm ] }

    // OptionDescription

    let optionArg longForm reader : OptionDescription =
        { shortForms = []
          longForms = [ longForm ]
          multiValued = None
          defaultText = None
          defaultValue = None
          reader = reader }

    let withLongOption longForm d : OptionDescription =
        { d with
              longForms = d.longForms @ [ longForm ] }

    let withShortOption shortForm d : OptionDescription =
        { d with
              shortForms = d.shortForms @ [ shortForm ] }

    let withOptionMultiValued multiValued d : OptionDescription = { d with multiValued = multiValued }

    let withOptionDefault text value d : OptionDescription =
        { d with
              defaultText = Some text
              defaultValue = Some value }

    // PositionalDescription

    let positionalArg priority reader : PositionalDescription =
        { priority = priority
          multiValued = None
          defaultText = None
          defaultValue = None
          reader = reader }

    let withPositionalMultiValued multiValued d : PositionalDescription = { d with multiValued = multiValued }

    let withPositionalDefault text value d : PositionalDescription =
        { d with
              defaultText = Some text
              defaultValue = Some value }

    // VerbDescription

    let verbArg verb =
        { verb = verb
          isDefault = false
          arguments = []
          examples = []
          usages = []
          execute = None }

    let defaultVerbArg verb = { verbArg verb with isDefault = true }
    let withExecute execute d = { d with execute = execute }
    let withExamples examples d = { d with examples = examples }
    let withUsages usages d = { d with usages = usages }

    let withExample example d =
        { d with
              examples = d.examples @ [ example ] }

    let withUsage usage d =
        { d with usages = d.usages @ [ usage ] }

    let withArg arg g : VerbDescription =
        { g with
              arguments = g.arguments @ [ arg ] }

    //let groupArg: GroupDescription = { arguments = [] }

    //let withGroupArg arg g : GroupDescription =
    //    { g with
    //          arguments = g.arguments @ [ arg ] }

    let withFlagArg longForm shortForm summary =
        withArg (
            let desc = summarized summary
            let arg = optional longForm
            let opt = flagArg longForm shortForm
            CommandLineFlag(desc, arg, opt)
        )

    let withLongFlagArg longForm summary =
        withArg (
            let desc = summarized summary
            let arg = optional longForm
            let opt = longFlagArg longForm
            CommandLineFlag(desc, arg, opt)
        )

    let withOptionArg longForm shortForm summary reader =
        withArg (
            let desc = summarized summary
            let arg = optional longForm

            let opt =
                optionArg longForm reader
                |> withShortOption shortForm

            CommandLineOption(desc, arg, opt)
        )

    let withLongOptionArg longForm summary reader =
        withArg (
            let desc = summarized summary
            let arg = optional longForm
            let opt = optionArg longForm reader
            CommandLineOption(desc, arg, opt)
        )

    let withOptionMultiArg longForm shortForm summary reader =
        withArg (
            let desc = summarized summary
            let arg = optional longForm

            let opt =
                optionArg longForm reader
                |> withShortOption shortForm
                |> withOptionMultiValued multiValued

            CommandLineOption(desc, arg, opt)
        )

    let withLongOptionMultiArg longForm summary reader =
        withArg (
            let desc = summarized summary
            let arg = optional longForm

            let opt =
                optionArg longForm reader
                |> withOptionMultiValued multiValued

            CommandLineOption(desc, arg, opt)
        )

    let withOptionalPositionalArg priority key summary reader =
        withArg (
            let desc = summarized summary
            let arg = optional key
            let pos = positionalArg priority reader
            CommandLinePositional(desc, arg, pos)
        )

    let withPositionalArg priority key summary reader =
        withArg (
            let desc = summarized summary
            let arg = required key
            let pos = positionalArg priority reader
            CommandLinePositional(desc, arg, pos)
        )

    // Create Metadata Root

    [<Literal>]
    let DefaultHelpFlagKey = "help"

    [<Literal>]
    let DefaultExamplesFlagKey = "examples"

    [<Literal>]
    let DefaultVersionFlagKey = "version"

    let create desc verb : CommandLineMetadata =
        { desc = desc
          verb = verb
          helpFlagKey = Some DefaultHelpFlagKey
          examplesFlagKey = Some DefaultExamplesFlagKey
          versionFlagKey = Some DefaultVersionFlagKey }

    // Common options

    let helpArg =
        let desc =
            elementDescription |> withSummary "Display help."

        let argDesc = required DefaultHelpFlagKey
        let flagDesc = flagArg "help" 'h'

        CommandLineFlag(desc, argDesc, flagDesc)

    let withHelpArg = withArg helpArg

    let versionArg =
        let desc =
            elementDescription
            |> withSummary "Display tool version."

        let argDesc = required DefaultVersionFlagKey
        let flagDesc = longFlagArg "version"

        CommandLineFlag(desc, argDesc, flagDesc)

    let withVersionArg = withArg versionArg

    let examplesArg =
        let desc =
            elementDescription
            |> withSummary "Print examples when showing command's help."

        let argDesc = required DefaultExamplesFlagKey
        let flagDesc = longFlagArg "examples"

        CommandLineFlag(desc, argDesc, flagDesc)

    let withExamplesArg = withArg examplesArg
