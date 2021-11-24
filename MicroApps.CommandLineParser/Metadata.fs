namespace MicroApps.CommandLineParser

open System
open Preamble
open System.IO

type ValueReader =
    { tokenCount: int
      typeName: string
      read: string list -> Result<obj, string> }

type ElementDesc =
    { name: string
      summary: string
      description: string }

type MultiValue =
    { minCount: int option
      maxCount: int option }

type ExecuteVerbContext =
    { services: IServiceProvider
      showHelp: string list -> unit
      valueMap: Map<string, obj> }

type ExecuteVerbFunc = ExecuteVerbContext -> Async<CommandLineExecuteResult>

type GroupDesc =
    { desc: ElementDesc
      showSummary: bool
      usages: string list
      showDefaultUsage: bool
      flags: FlagDesc list
      options: OptionDesc list
      arguments: ArgumentDesc list
      verbs: VerbDesc list
      examples: string list }

and FlagDesc =
    { key: string
      desc: ElementDesc
      shortForms: char list
      longForms: string list }

and OptionDesc =
    { key: string
      isRequired: bool
      desc: ElementDesc
      shortForms: char list
      longForms: string list
      multiValued: MultiValue option
      defaultValue: (obj * string) option
      reader: ValueReader }

and ArgumentDesc =
    { key: string
      isRequired: bool
      desc: ElementDesc
      multiValued: MultiValue option
      defaultValue: (obj * string) option
      reader: ValueReader }

and VerbDesc =
    { key: string
      groups: GroupDesc list
      execute: ExecuteVerbFunc option }

type CommandLineMetadata =
    { version: string
      helpFlagKey: string option
      detailedHelpFlagKey: string option
      versionFlagKey: string option
      verb: VerbDesc }

module Metadata =
    let tryGetOptional<'t> key (map: Map<string, obj>) : 't option =
        match Map.tryFind key map with
        | None -> None
        | Some obj ->
            match obj with
            | :? 't as t -> Some t
            | value -> failwith $"Invalid type found at key '%s{key}': %A{value}"

    let tryGetOptionalList<'t> key map =
        tryGetOptional<obj list> key map
        |> Option.map (List.map (fun o -> o :?> 't))

    let tryGetOr (defaultValue: 't) key map =
        tryGetOptional<'t> key map
        |> Option.defaultValue defaultValue

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
          typeName = "string"
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

    let enumReader<'enum when 'enum: struct and 'enum: (new: unit -> 'enum) and 'enum :> System.Enum> typeName =
        tryParseReader typeName System.Enum.TryParse<'enum>

    // ElementDesc

    let elementDesc: ElementDesc =
        { name = ""
          summary = ""
          description = "" }

    let named name = { elementDesc with name = name }

    let summarized summary = { elementDesc with summary = summary }

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

    // GroupDesc

    let group: GroupDesc =
        { desc = elementDesc
          showSummary = true
          usages = []
          showDefaultUsage = false
          flags = []
          options = []
          arguments = []
          verbs = []
          examples = [] }

    let withGroupName name d : GroupDesc =
        { d with
              desc = { d.desc with name = name } }

    let withGroupSummary summary d : GroupDesc =
        { d with
              desc = { d.desc with summary = summary } }

    let withGroupDescription description d : GroupDesc =
        { d with
              desc =
                  { d.desc with
                        description = description } }

    let hideSummary d =
        { d with showSummary = false }

    let withUsage usage d =
        { d with usages = d.usages @ [ usage ] }

    let withDefaultUsage d =
        { d with showDefaultUsage = true }

    let withExample example d =
        { d with
              examples = d.examples @ [ example ] }

    let withFlag flag d = { d with flags = d.flags @ [ flag ] }

    let withOption option d =
        { d with
              options = d.options @ [ option ] }

    let withArgument argument d =
        { d with
              arguments = d.arguments @ [ argument ] }

    let withVerb verb d = { d with verbs = d.verbs @ [ verb ] }

    // FlagDesc

    let flag longForm shortForm summary : FlagDesc =
        { key = longForm
          desc = summarized summary
          shortForms = [ shortForm ]
          longForms = [ longForm ] }

    let longFlag longForm summary : FlagDesc =
        { key = longForm
          desc = summarized summary
          shortForms = []
          longForms = [ longForm ] }

    let withFlagName name f : FlagDesc = { f with desc = f.desc |> withName name }
    let withFlagSummary summary f : FlagDesc = { f with desc = f.desc |> withSummary summary }
    let withFlagDescription description f : FlagDesc = { f with desc = f.desc |> withDescription description }

    let withFlagKey key f : FlagDesc = { f with key = key }

    let withLongFlag longForm f : FlagDesc =
        { f with
              longForms = f.longForms @ [ longForm ] }

    let withShortFlag shortForm f : FlagDesc =
        { f with
              shortForms = f.shortForms @ [ shortForm ] }

    // OptionDesc

    let option longForm shortForm reader summary : OptionDesc =
        { key = longForm
          isRequired = false
          desc = summarized summary
          shortForms = [ shortForm ]
          longForms = [ longForm ]
          multiValued = None
          defaultValue = None
          reader = reader }

    let longOption longForm reader summary : OptionDesc =
        { key = longForm
          isRequired = false
          desc = summarized summary
          shortForms = []
          longForms = [ longForm ]
          multiValued = None
          defaultValue = None
          reader = reader }

    let withOptionName name f : OptionDesc = { f with desc = f.desc |> withName name }
    let withOptionSummary summary f : OptionDesc = { f with desc = f.desc |> withSummary summary }
    let withOptionDescription description f : OptionDesc = { f with desc = f.desc |> withDescription description }

    let withOptionKey key f : OptionDesc = { f with key = key }

    let asRequiredOption d : OptionDesc = { d with isRequired = true }

    let withLongOption longForm d : OptionDesc =
        { d with
              longForms = d.longForms @ [ longForm ] }

    let withShortOption shortForm d : OptionDesc =
        { d with
              shortForms = d.shortForms @ [ shortForm ] }

    let withOptionMultiValued multiValued d : OptionDesc = { d with multiValued = multiValued }

    let withOptionDefault text value d : OptionDesc =
        { d with
              defaultValue = Some(value, text) }

    let withOptionDefaultValue value = withOptionDefault (string value) value

    // ArgumentDesc

    let argument key reader summary : ArgumentDesc =
        { key = key
          isRequired = false
          desc = summarized summary
          multiValued = None
          defaultValue = None
          reader = reader }

    let asRequiredArgument d : ArgumentDesc = { d with isRequired = true }

    let withArgumentMultiValued multiValued d : ArgumentDesc = { d with multiValued = multiValued }

    let withArgumentDefault text value d : ArgumentDesc =
        { d with
              defaultValue = Some(value, text) }

    let withArgumentDefaultValue value =
        withArgumentDefault (string value) value

    // VerbDesc

    let verb key : VerbDesc =
        { key = key
          execute = None
          groups = [] }

    let withExecute execute d = { d with execute = Some execute }

    let withGroup group d : VerbDesc =
        { d with groups = d.groups @ [ group ] }

    // Create Metadata Root

    [<Literal>]
    let DefaultHelpFlagKey = "help"

    [<Literal>]
    let DefaultDetailedHelpFlagKey = "detailed-help"

    [<Literal>]
    let DefaultVersionFlagKey = "version"

    let create version verb : CommandLineMetadata =
        { version = version
          verb = verb
          helpFlagKey = Some DefaultHelpFlagKey
          detailedHelpFlagKey = Some DefaultDetailedHelpFlagKey
          versionFlagKey = Some DefaultVersionFlagKey }

    // Common options

    let helpFlag =
        flag DefaultHelpFlagKey 'h' "Display help."

    let withHelpFlag = withFlag helpFlag

    let detailedHelpFlag =
        longFlag DefaultDetailedHelpFlagKey "Display detailed help, with descriptions and examples."

    let withDetailedHelpFlag = withFlag detailedHelpFlag

    let versionFlag =
        longFlag DefaultVersionFlagKey "Display tool version."

    let withVersionFlag = withFlag versionFlag
