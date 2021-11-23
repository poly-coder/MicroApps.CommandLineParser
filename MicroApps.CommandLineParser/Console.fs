module MicroApps.CommandLineParser.Console

open System
open Preamble

let black = ConsoleColor.Black
let blue = ConsoleColor.Blue
let cyan = ConsoleColor.Cyan
let darkBlue = ConsoleColor.DarkBlue
let darkCyan = ConsoleColor.DarkCyan
let darkGray = ConsoleColor.DarkGray
let darkGreen = ConsoleColor.DarkGreen
let darkMagenta = ConsoleColor.DarkMagenta
let darkRed = ConsoleColor.DarkRed
let darkYellow = ConsoleColor.DarkYellow
let gray = ConsoleColor.Gray
let green = ConsoleColor.Green
let magenta = ConsoleColor.Magenta
let red = ConsoleColor.Red
let white = ConsoleColor.White
let yellow = ConsoleColor.Yellow

let writelnC color text =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    Console.WriteLine(text: string)
    Console.ForegroundColor <- oldColor

let writeln text = writelnC gray text

let writeC color text =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    Console.Write(text: string)
    Console.ForegroundColor <- oldColor

let write text = writeC gray text

type ColumnTextAlignment =
    | Left = 1
    | Center = 2
    | Right = 3

type WriteTableColumnOptions =
    { alignment: ColumnTextAlignment option
      minWidth: int option
      separator: string option }

let columns =
    { alignment = None
      minWidth = None
      separator = None }

let withAlignment alignment options =
    { options with
          alignment = Some alignment }

let withInheritedAlignment options = { options with alignment = None }

let withMinWidth minWidth options =
    { options with
          minWidth = Some minWidth }

let withInheritedMinWidth options = { options with minWidth = None }

let withSeparator separator options =
    { options with
          separator = Some separator }

let withInheritedSeparator options = { options with separator = None }

type WriteTableOptions =
    { separator: string
      separatorStart: string
      separatorEnd: string
      alignment: ColumnTextAlignment
      minWidth: int
      columns: Map<int, WriteTableColumnOptions> }

let markdownTableOptions =
    { separator = " | "
      separatorStart = "| "
      separatorEnd = " |"
      alignment = ColumnTextAlignment.Left
      minWidth = 0
      columns = Map.empty }

let cleanTableOptions =
    { separator = " "
      separatorStart = ""
      separatorEnd = ""
      alignment = ColumnTextAlignment.Left
      minWidth = 0
      columns = Map.empty }

let withDefaultAlignment alignment options : WriteTableOptions =
    { options with
          alignment = alignment }

let withDefaultMinWidth minWidth options : WriteTableOptions =
    { options with
          minWidth = minWidth }

let withColumn index column options : WriteTableOptions =
    { options with
          columns =
              options.columns
              |> Map.add index column }

let withColumnUpdate updateFn index options =
    let updateFn =
        (function
        | Some o -> o
        | None -> columns)
        >> updateFn
        >> Some

    let map =
        options.columns |> Map.update index updateFn

    { options with columns = map }

let withColumnAlignment alignment =
    withColumnUpdate (withAlignment alignment)

let withColumnMinWidth minWidth =
    withColumnUpdate (withMinWidth minWidth)

let withColumnSeparator separator =
    withColumnUpdate (withSeparator separator)

let getColumnOption colFn defaultFn index options =
    options.columns
    |> Map.tryFind index
    |> function
        | Some columns ->
            colFn columns
            |> Option.defaultValue (defaultFn options)
        | None -> defaultFn options

let getColumnAlignment =
    getColumnOption (fun c -> c.alignment) (fun o -> o.alignment)

let getColumnMinWidth =
    getColumnOption (fun c -> c.minWidth) (fun o -> o.minWidth)

let getColumnSeparator =
    getColumnOption (fun c -> c.separator) (fun o -> o.separator)

let writeTable (options: WriteTableOptions) (rows: string list list) =
    let rowCount = List.length rows

    if rowCount = 0 then
        ()
    else
        let columnCount = List.length (List.head rows)

        if columnCount = 0 then
            ()
        else
            let columnSizes =
                Array.init columnCount (fun index -> getColumnMinWidth index options)

            for ri = 0 to rowCount - 1 do
                let row = rows.[ri]

                for ci = 0 to columnCount - 1 do
                    let column = row.[ci]
                    let width = String.length column
                    columnSizes.[ci] <- max width columnSizes.[ci]

            for ri = 0 to rowCount - 1 do
                let row = rows.[ri]

                write options.separatorStart

                for ci = 0 to columnCount - 1 do
                    let column = row.[ci]
                    let columnWidth = columnSizes.[ci]
                    let alignment = getColumnAlignment ci options
                    let separator = getColumnSeparator ci options

                    let text =
                        match alignment with
                        | ColumnTextAlignment.Center -> column |> String.padCenterWith ' ' columnWidth
                        | ColumnTextAlignment.Right -> column |> String.padLeftWith ' ' columnWidth
                        | _ -> column |> String.padRightWith ' ' columnWidth

                    write text

                    if ci < columnCount - 1 then
                        write separator

                write options.separatorEnd
                writeln ""
