namespace MicroApps.CommandLineParser

open System
open System.Reflection
open Preamble

//[<AttributeUsage(
//    AttributeTargets.Class ||| 
//    AttributeTargets.Property ||| 
//    AttributeTargets.Field, 
//    AllowMultiple = false, 
//    Inherited = true)>]
//type DescriptionsAttribute() =
//    member val Name = "" with get, set
//    member val Summary = "" with get, set
//    member val Description = "" with get, set

//    member this.AsDescription() =
//        Metadata.elementDescription
//            |> if isNullOrWS this.Name then id else Metadata.withName this.Name
//            |> if isNullOrWS this.Summary then id else Metadata.withSummary this.Summary
//            |> if isNullOrWS this.Description then id else Metadata.withDescription this.Description

//    static member TryGetDescription(source: ICustomAttributeProvider) =
//        source.GetCustomAttributes(true)
//        |> Seq.ofType<DescriptionsAttribute>
//        |> Seq.tryHead
//        |> Option.map (fun attr -> attr.AsDescription())

//    static member GetDescriptionOrDefault(source: ICustomAttributeProvider) =
//        DescriptionsAttribute.TryGetDescription(source)
//        |> Option.defaultValue Metadata.elementDescription

//[<AttributeUsage(
//    AttributeTargets.Class ||| 
//    AttributeTargets.Property ||| 
//    AttributeTargets.Field, 
//    AllowMultiple = false, 
//    Inherited = true)>]
//type MultiValuedAttribute() =
//    member val MinCount = -1 with get, set
//    member val MaxCount = -1 with get, set

//    member this.AsMultiValue() =
//        let minCount = if this.MinCount < 0 then None else Some this.MinCount
//        let maxCount = if this.MaxCount < 0 then None else Some this.MaxCount
//        { minCount = minCount
//          maxCount = maxCount }

//    static member GetMultiValue(source: ICustomAttributeProvider) =
//        source.GetCustomAttributes(true)
//        |> Seq.ofType<MultiValuedAttribute>
//        |> Seq.tryHead
//        |> Option.map (fun attr -> attr.AsMultiValue())

//[<AttributeUsage(
//    AttributeTargets.Class ||| 
//    AttributeTargets.Property ||| 
//    AttributeTargets.Field, 
//    AllowMultiple = false, 
//    Inherited = true)>]
//[<AbstractClass>]
//type ArgumentAttribute () =
//    class end
