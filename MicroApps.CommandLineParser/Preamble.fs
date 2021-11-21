module MicroApps.CommandLineParser.Preamble

open System

let isNullOrWS = String.IsNullOrWhiteSpace
let isNotNullOrWS = isNullOrWS >> not

module String =
    let padLeftWith (char: char) width text =
        let textLength = String.length text
        if textLength >= width then
            text
        else
            let pad = String.replicate (width - textLength) (string char)
            $"%s{pad}%s{text}"

    let padRightWith (char: char) width text =
        let textLength = String.length text
        if textLength >= width then
            text
        else
            let pad = String.replicate (width - textLength) (string char)
            $"%s{text}%s{pad}"

    let padCenterWith (char: char) width text =
        let textLength = String.length text
        if textLength >= width then
            text
        else
            let leftWidth = (width - textLength) / 2
            let rightWidth = width - textLength - leftWidth
            let leftPad = String.replicate leftWidth (string char)
            let rightPad = String.replicate rightWidth (string char)
            $"%s{leftPad}%s{text}%s{rightPad}"

module Seq =
    let ofType<'t> (source: obj seq) : 't seq =
        source
        |> Seq.collect (function
            | :? 't as t -> seq { yield t }
            | _ -> Seq.empty)

module Map =
    let update key fn map =
        Map.tryFind key map
        |> fn
        |> function
            | Some value -> map |> Map.add key value
            | None -> map |> Map.remove key
