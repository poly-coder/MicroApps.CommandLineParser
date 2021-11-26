namespace MicroApps.CommandLineParser

open System
open System.Reflection
open System.Text.RegularExpressions
open FSharp.Reflection
open Preamble
open Metadata

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

    let simpleTypeReaders =
        readOnlyDict [
            typeof<string>, stringReader
            typeof<char>, charReader
            typeof<int8>, int8Reader
            typeof<int16>, int16Reader
            typeof<int32>, int32Reader
            typeof<int64>, int64Reader
            typeof<uint8>, uint8Reader
            typeof<uint16>, uint16Reader
            typeof<uint32>, uint32Reader
            typeof<uint64>, uint64Reader
            typeof<single>, singleReader
            typeof<double>, doubleReader
            typeof<decimal>, decimalReader
            typeof<DateTime>, dateTimeReader
            typeof<DateTimeOffset>, dateTimeOffsetReader
            typeof<TimeSpan>, timeSpanReader
        ]

    let listDefinitionTypes = [
        typedefof<seq<_>>
        typedefof<list<_>>
        typedefof<array<_>>
        typedefof<ResizeArray<_>>
    ]

    let optionalDefinitionTypes = [
        typedefof<option<_>>
        typedefof<Nullable<_>>
    ]

    let rec getReaderForBaseType (propertyType: Type) =
        match simpleTypeReaders.TryGetValue propertyType with
        | true, reader -> reader, true, false, propertyType
        | false, _ ->
            if propertyType.IsGenericType then
                let definition = propertyType.GetGenericTypeDefinition()
                if List.contains definition listDefinitionTypes then
                    let baseType = propertyType.GetGenericArguments().[0]
                    let reader, _, _, underlyingType = getReaderForBaseType baseType
                    reader, false, true, underlyingType
                elif List.contains definition optionalDefinitionTypes then
                    let baseType = propertyType.GetGenericArguments().[0]
                    let reader, _, _, underlyingType = getReaderForBaseType baseType
                    reader, false, false, underlyingType
                else
                    failwith $"Unsupported type definition for reader: %A{definition}"
            else
                failwith $"Unsupported type for reader: %A{propertyType}"
                    
    let getReaderForProperty (property: PropertyInfo) =
        let baseReader, isRequired, isMultiValued, underlyingType =
            getReaderForBaseType property.PropertyType

        let reader =
            match TypeNameAttribute.TryGetTypeName property with
            | Some typeName -> { baseReader with typeName = typeName }
            | _ -> baseReader

        let multiValued =
            if isMultiValued then
                match MultiValuedAttribute.TryGetMultiValued(property) with
                | Some x -> Some x
                | None -> multiValued
            else
                match MultiValuedAttribute.TryGetMultiValued(property) with
                | Some x -> failwith $"Property %s{property.Name} is marked as multi-valued but it's type does not allow for it"
                | None -> None

        reader, isRequired, multiValued, underlyingType

    let createOptionFromProperty (property: PropertyInfo) =
        let setKey =
            OptionKeyAttribute.TryGetOptionKey property
            |> function
                | Some key -> withOptionKey key
                | _ -> id

        let setLongForms option =
            AlternativeLongFormsAttribute.GetAlternativeLongForms property
            |> Seq.fold (fun acc form -> withLongOption form acc) option

        let setShortForms option =
            AlternativeShortFormsAttribute.GetAlternativeShortForms property
            |> Seq.fold (fun acc form -> withShortOption form acc) option

        let reader, isRequired, multiValued, _ =
            getReaderForProperty property

        let setIsRequired = if isRequired then asRequiredOption else id

        let setMultiValued option : OptionDesc =
            { option with multiValued = multiValued }

        longOption
            (LongFormAttribute.GetLongForm property)
            reader
            (SummaryAttribute.GetSummary property)
        |> withOptionName (NameAttribute.GetName property)
        |> withOptionDescription (DescriptionAttribute.GetDescription property)
        |> setKey
        |> setIsRequired
        |> setMultiValued
        |> setLongForms
        |> setShortForms

    let isFlagProperty (property: PropertyInfo) =
        property.PropertyType = typeof<bool> &&
        property.GetIndexParameters().Length = 0

    let isArgumentProperty (property: PropertyInfo) =
        property.PropertyType <> typeof<bool> &&
        ArgumentAttribute.IsArgument property &&
        property.GetIndexParameters().Length = 0

    let isOptionProperty (property: PropertyInfo) =
        property.PropertyType <> typeof<bool> &&
        isArgumentProperty property &&
        property.GetIndexParameters().Length = 0

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
            |> Seq.filter isFlagProperty
            |> Seq.map createFlagFromProperty
            |> Seq.fold (fun acc flag -> withFlag flag acc) group

        let setOptions group =
            optionsClass.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
            |> Seq.filter isOptionProperty
            |> Seq.map createOptionFromProperty
            |> Seq.fold (fun acc opt -> withOption opt acc) group

        group
        |> withGroupName (NameAttribute.GetName optionsClass)
        |> withGroupSummary (SummaryAttribute.GetSummary optionsClass)
        |> withGroupDescription (DescriptionAttribute.GetDescription optionsClass)
        |> setHideSummary
        |> setDefaultUsage
        |> setUsages
        |> setExamples
        |> setFlags
        |> setOptions

    let getLongestConstructor (verbClass: Type) =
        let constructors =
            verbClass.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)

        if constructors.Length = 0 then
            failwith $"Type %s{verbClass.Name} has no public instance constructor"

        constructors
        |> Seq.map (fun c -> c, c.GetParameters())
        |> Seq.maxBy (fun (c, ps) -> ps.Length)

    let addVerbGroups (verbClass: Type) verbDesc =
        getLongestConstructor verbClass
        |> snd
        |> Seq.collect
            (fun parameter ->
                if OptionGroupAttribute.IsOptionGroup parameter then
                    [ parameter.ParameterType ]
                else
                    [])
        |> Seq.map createGroupFromClass
        |> Seq.fold (fun acc group -> acc |> withGroup group) verbDesc

    let listOfSeqMethod =
        match <@ List.ofSeq [1] @> with
        | Quotations.Patterns.Call(_, ofSeqInt, _) -> ofSeqInt
        | _ -> failwith "Unexpected"
        |> fun ofSeqInt -> ofSeqInt.GetGenericMethodDefinition()

    let listGetEmptyMethod =
        match <@ List.empty<int> @> with
        | Quotations.Patterns.Call(_, getEmpty, _) -> getEmpty
        | _ -> failwith "Unexpected"
        |> fun getEmpty -> getEmpty.GetGenericMethodDefinition()

    let createOptionsObject parameterType (valueMap: Map<string, obj>) =
        if FSharpType.IsRecord parameterType then
            let bindingFlags = BindingFlags.Public ||| BindingFlags.Instance
            FSharpType.GetRecordFields(parameterType, bindingFlags)
            |> Seq.map (fun property ->
                let key = OptionKeyAttribute.GetOptionKey property

                if isFlagProperty property then
                    match valueMap |> Map.tryFind key with
                    | Some value ->
                        value
                    | None -> false
                elif isOptionProperty property then
                    let _, isOptional, multiValued, underlyingType = getReaderForProperty property
                    match valueMap |> Map.tryFind key with
                    | Some value ->
                        if isOptional then
                            (Some value) :> obj
                        elif multiValued |> Option.isSome then
                            value
                        else
                            value :?> obj list
                            |> (fun ls ->
                                let ofSeq = listOfSeqMethod.MakeGenericMethod(underlyingType)
                                ofSeq.Invoke(null, [| ls |]))
                    | None ->
                        if isOptional then
                            None :> obj
                        elif multiValued |> Option.isSome then
                            let getEmpty = listGetEmptyMethod.MakeGenericMethod(underlyingType)
                            getEmpty.Invoke(null, Array.empty)
                        else
                            null
                else null
            )
            |> Seq.toArray
            |> fun values -> FSharpValue.MakeRecord(parameterType, values, bindingFlags)
        else
            failwith $"Cannot create options for type %A{parameterType}"

    let activateCommandLineExecute (classType: Type) (services: IServiceProvider) (valueMap: Map<string, obj>) =
        let args =
            getLongestConstructor classType
            |> snd
            |> Seq.map
                (fun parameter ->
                    if OptionGroupAttribute.IsOptionGroup parameter then
                        createOptionsObject parameter.ParameterType valueMap
                    else
                        services.GetService(parameter.ParameterType))
            |> Seq.toArray
            
        let instance = Activator.CreateInstance(classType, args)
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
