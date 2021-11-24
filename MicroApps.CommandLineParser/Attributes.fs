namespace MicroApps.CommandLineParser

open System
open System.Reflection
open System.Text.RegularExpressions
open Preamble

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

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)>]
type ArgumentAttribute() =
    inherit Attribute()

    static member TryGetAttribute(source: ICustomAttributeProvider) =
        source.GetCustomAttributes(true)
        |> Seq.ofType<ArgumentAttribute>
        |> Seq.tryHead

    static member IsArgument(source: ICustomAttributeProvider) =
        ArgumentAttribute.TryGetAttribute(source)
        |> Option.isSome

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
