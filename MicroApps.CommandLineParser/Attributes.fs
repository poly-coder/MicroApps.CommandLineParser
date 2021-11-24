namespace MicroApps.CommandLineParser

open System
open System.Reflection
open System.Text.RegularExpressions
open Preamble

open Metadata

[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Property,
                 AllowMultiple = false,
                 Inherited = true)>]
type NameAttribute(name: string) =
    inherit Attribute()

    member val Name = name with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<NameAttribute>
        |> Seq.tryHead

    static member GetName(source: ICustomAttributeProvider) =
        NameAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.Name)
        |> Option.defaultValue ""

[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Property,
                 AllowMultiple = false,
                 Inherited = true)>]
type SummaryAttribute(summary: string) =
    inherit Attribute()

    member val Summary = summary with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<SummaryAttribute>
        |> Seq.tryHead

    static member GetSummary(source: ICustomAttributeProvider) =
        SummaryAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.Summary)
        |> Option.defaultValue ""

[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Property,
                 AllowMultiple = false,
                 Inherited = true)>]
type DescriptionAttribute(description: string) =
    inherit Attribute()

    member val Description = description with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<DescriptionAttribute>
        |> Seq.tryHead

    static member GetDescription(source: ICustomAttributeProvider) =
        DescriptionAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.Description)
        |> Option.defaultValue ""

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type HideSummaryAttribute() =
    inherit Attribute()

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<HideSummaryAttribute>
        |> Seq.tryHead

    static member HideSummary(source: ICustomAttributeProvider) =
        HideSummaryAttribute.TryGetAttribute(source)
        |> Option.isSome

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)>]
type TypeNameAttribute(typeName: string) =
    inherit Attribute()

    member val TypeName = typeName with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<TypeNameAttribute>
        |> Seq.tryHead

    static member TryGetTypeName(source: ICustomAttributeProvider) =
        TypeNameAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.TypeName)

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)>]
type MultiValuedAttribute() =
    inherit Attribute()

    member val MinCount = -1 with get, set
    member val MaxCount = -1 with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<MultiValuedAttribute>
        |> Seq.tryHead

    static member TryGetMultiValued(source: ICustomAttributeProvider) =
        MultiValuedAttribute.TryGetAttribute(source)
        |> Option.map
            (fun attr ->
                let minCount =
                    if attr.MinCount < 0 then
                        None
                    else
                        Some attr.MinCount

                let maxCount =
                    if attr.MaxCount < 0 then
                        None
                    else
                        Some attr.MaxCount

                { minCount = minCount
                  maxCount = maxCount })

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type UsageAttribute(usage: string) =
    inherit Attribute()

    member val Usage = usage with get, set

    static member GetAllAttributes(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<UsageAttribute>

    static member GetUsages(source: ICustomAttributeProvider) =
        UsageAttribute.GetAllAttributes(source)
        |> Seq.map (fun attr -> attr.Usage)
        |> Seq.toList
        
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type ExampleAttribute(example: string) =
    inherit Attribute()

    member val Example = example with get, set

    static member GetAllAttributes(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<ExampleAttribute>

    static member GetExamples(source: ICustomAttributeProvider) =
        ExampleAttribute.GetAllAttributes(source)
        |> Seq.map (fun attr -> attr.Example)
        |> Seq.toList

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type DefaultUsageAttribute() =
    inherit Attribute()

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<DefaultUsageAttribute>
        |> Seq.tryHead

    static member ShowDefaultUsage(source: ICustomAttributeProvider) =
        DefaultUsageAttribute.TryGetAttribute(source)
        |> Option.isSome

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)>]
type LongFormAttribute(longForm: string) =
    inherit Attribute()

    member val LongForm = longForm with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<LongFormAttribute>
        |> Seq.tryHead

    static member TryGetLongForm(source: ICustomAttributeProvider) =
        LongFormAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.LongForm)

    static member GetLongForm(property: PropertyInfo) =
        let toKebabCase name =
            Regex.Replace(name, @"(?<part>[a-zA-Z][a-z0-9]*)", fun (m: Match) ->
                let prefix = if m.Index > 0 then "-" else ""
                let word = m.Value.ToLowerInvariant()
                $"%s{prefix}%s{word}")
        LongFormAttribute.TryGetLongForm(property)
        |> Option.defaultValue (toKebabCase property.Name)

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)>]
type AlternativeLongFormsAttribute([<ParamArray>] longForms: string []) =
    inherit Attribute()

    member val AlternativeLongForms = longForms with get, set

    static member GetAllAttributes(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<AlternativeLongFormsAttribute>

    static member GetAlternativeLongForms(source: ICustomAttributeProvider) =
        AlternativeLongFormsAttribute.GetAllAttributes(source)
        |> Seq.collect (fun attr -> attr.AlternativeLongForms)
        |> Seq.toList

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true, Inherited = true)>]
type AlternativeShortFormsAttribute([<ParamArray>] shortForms: char []) =
    inherit Attribute()

    member val AlternativeShortForms = shortForms with get, set

    static member GetAllAttributes(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<AlternativeShortFormsAttribute>

    static member GetAlternativeShortForms(source: ICustomAttributeProvider) =
        AlternativeShortFormsAttribute.GetAllAttributes(source)
        |> Seq.collect (fun attr -> attr.AlternativeShortForms)
        |> Seq.toList

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = true)>]
type SubCommandsAttribute([<ParamArray>] subCommands: Type []) =
    inherit Attribute()

    member val SubCommands = subCommands with get, set

    static member GetAllAttributes(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<SubCommandsAttribute>

    static member GetSubCommands(source: ICustomAttributeProvider) =
        SubCommandsAttribute.GetAllAttributes(source)
        |> Seq.collect (fun attr -> attr.SubCommands)
        |> Seq.toList

[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Property,
                 AllowMultiple = false,
                 Inherited = true)>]
type OptionKeyAttribute(optionKey: string) =
    inherit Attribute()

    member val OptionKey = optionKey with get, set

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<OptionKeyAttribute>
        |> Seq.tryHead

    static member TryGetOptionKey(source: ICustomAttributeProvider) =
        OptionKeyAttribute.TryGetAttribute(source)
        |> Option.map (fun attr -> attr.OptionKey)

[<AttributeUsage(AttributeTargets.Parameter, AllowMultiple = false, Inherited = true)>]
type OptionGroupAttribute() =
    inherit Attribute()

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<OptionGroupAttribute>
        |> Seq.tryHead

    static member IsOptionGroup(source: ICustomAttributeProvider) =
        OptionGroupAttribute.TryGetAttribute(source)
        |> Option.isSome

type ICommandLineExecute =
    abstract Execute: unit -> Async<int>

type ReflectionMetadataProvider(mainVerbType: Type, version: string) =

    let mutable metadata: CommandLineMetadata option = None

    let createFlagFromProperty (property: PropertyInfo) =
        let setKey =
            OptionKeyAttribute.TryGetOptionKey property
            |> function
                | Some key -> withFlagKey key
                | _ -> id

        let setLongForms flag =
            AlternativeLongFormsAttribute.GetAlternativeLongForms property
            |> Seq.fold (fun acc form -> withLongFlag form acc) flag

        let setShortForms flag =
            AlternativeShortFormsAttribute.GetAlternativeShortForms property
            |> Seq.fold (fun acc form -> withShortFlag form acc) flag

        longFlag
            (LongFormAttribute.GetLongForm property)
            (SummaryAttribute.GetSummary property)
        |> withFlagName (NameAttribute.GetName property)
        |> withFlagDescription (DescriptionAttribute.GetDescription property)
        |> setKey
        |> setLongForms
        |> setShortForms

    let createGroupFromClass (optionsClass: Type) =
        let setDefaultUsage =
            if DefaultUsageAttribute.ShowDefaultUsage optionsClass then
                withDefaultUsage
            else
                id

        let setHideSummary =
            if HideSummaryAttribute.HideSummary optionsClass then
                hideSummary
            else
                id

        let setUsages group =
            UsageAttribute.GetUsages optionsClass
            |> Seq.fold (fun acc usage -> withUsage usage acc) group

        let setExamples group =
            ExampleAttribute.GetExamples optionsClass
            |> Seq.fold (fun acc usage -> withExample usage acc) group

        let setFlags group =
            optionsClass.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
            |> Seq.filter (fun prop ->
                prop.PropertyType = typeof<bool> &&
                prop.GetIndexParameters().Length = 0)
            |> Seq.map createFlagFromProperty
            |> Seq.fold (fun acc flag -> withFlag flag acc) group

        group
        |> withGroupName (NameAttribute.GetName optionsClass)
        |> withGroupSummary (SummaryAttribute.GetSummary optionsClass)
        |> withGroupDescription (DescriptionAttribute.GetDescription optionsClass)
        |> setHideSummary
        |> setDefaultUsage
        |> setUsages
        |> setExamples
        |> setFlags


    let addVerbGroups (verbClass: Type) =
        let constructors =
            verbClass.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)

        if constructors.Length = 0 then
            failwith $"Type %s{verbClass.Name} has no public instance constructor"

        fun verbDesc ->
            constructors
            |> Seq.map (fun c -> c.GetParameters())
            |> Seq.maxBy (fun ps -> ps.Length)
            |> Seq.collect
                (fun parameter ->
                    if OptionGroupAttribute.IsOptionGroup parameter then
                        [ parameter.ParameterType ]
                    else
                        [])
            |> Seq.map createGroupFromClass
            |> Seq.fold (fun acc group -> acc |> withGroup group) verbDesc

    let activateCommandLineExecute (classType: Type) (services: IServiceProvider) (valueMap: Map<string, obj>) =
        let instance = Activator.CreateInstance(classType)
        instance :?> ICommandLineExecute

    let getVerbKeyFromClass (verbClass: Type) =
        OptionKeyAttribute.TryGetOptionKey verbClass
        |> Option.defaultValue verbClass.Name

    let getExecuteFromClass (verbClass: Type) =
        if typeof<ICommandLineExecute>.IsAssignableFrom verbClass then
            let execute (context: ExecuteVerbContext) =
                async {
                    let instance =
                        activateCommandLineExecute verbClass context.services context.valueMap

                    let! exitCode = instance.Execute()

                    return { exitCode = exitCode }
                }

            Some execute
        else
            None

    let addVerbExecute (verbClass: Type) =
        match getExecuteFromClass verbClass with
        | Some execute -> withExecute execute
        | None -> id

    let createVerbFromClass isRoot (verbClass: Type) =
        let addGlobalGroup (verbClass: Type) =
            if isRoot then
                withGroup (
                    group
                    |> withGroupName "Global"
                    |> hideSummary
                    |> withHelpFlag
                    |> withDetailedHelpFlag
                    |> withVersionFlag
                )
            else
                id

        getVerbKeyFromClass verbClass
        |> verb
        |> addVerbGroups verbClass
        |> addVerbExecute verbClass
        |> addGlobalGroup verbClass

    let createMetadata () =
        let verbDesc = createVerbFromClass true mainVerbType
        create version verbDesc

    interface ICommandLineMetadataProvider with
        member this.GetMetadata() =
            let getOrCreate () =
                match metadata with
                | Some metadata -> metadata
                | None ->
                    let value = createMetadata ()
                    metadata <- Some value
                    value

            async { return getOrCreate () }
