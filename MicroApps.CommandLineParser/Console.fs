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
      minWidth: int option }

let columnOptions = { alignment = None; minWidth = None }

let withAlignment alignment options =
    { options with
          alignment = Some alignment }

let withInheritedAlignment options = { options with alignment = None }

let withMinWidth minWidth options =
    { options with
          minWidth = Some minWidth }

let withInheritedMinWidth options = { options with minWidth = None }

type WriteTableOptions =
    { columnSeparator: string
      columnSeparatorStart: string
      columnSeparatorEnd: string
      defaultAlignment: ColumnTextAlignment
      defaultMinWidth: int
      columnOptions: Map<int, WriteTableColumnOptions> }

let markdownTableOptions =
    { columnSeparator = " | "
      columnSeparatorStart = "| "
      columnSeparatorEnd = " |"
      defaultAlignment = ColumnTextAlignment.Left
      defaultMinWidth = 0
      columnOptions = Map.empty }

let cleanTableOptions =
    { columnSeparator = " "
      columnSeparatorStart = ""
      columnSeparatorEnd = ""
      defaultAlignment = ColumnTextAlignment.Left
      defaultMinWidth = 0
      columnOptions = Map.empty }

let withDefaultAlignment alignment options =
    { options with
          defaultAlignment = alignment }

let withDefaultMinWidth minWidth options =
    { options with
          defaultMinWidth = minWidth }

let withColumnOptions index columnOptions options =
    { options with
          columnOptions =
              options.columnOptions
              |> Map.add index columnOptions }

let withColumnUpdate updateFn index options =
    let updateFn =
        (function
        | Some o -> o
        | None -> columnOptions)
        >> updateFn
        >> Some

    let map =
        options.columnOptions |> Map.update index updateFn

    { options with columnOptions = map }

let withColumnAlignment alignment =
    withColumnUpdate (withAlignment alignment)

let withColumnMinWidth minWidth =
    withColumnUpdate (withMinWidth minWidth)

let getColumnOption colFn defaultFn index options =
    options.columnOptions
    |> Map.tryFind index
    |> function
        | Some columnOptions ->
            colFn columnOptions
            |> Option.defaultValue (defaultFn options)
        | None -> defaultFn options

let getColumnAlignment =
    getColumnOption (fun c -> c.alignment) (fun o -> o.defaultAlignment)

let getColumnMinWidth =
    getColumnOption (fun c -> c.minWidth) (fun o -> o.defaultMinWidth)

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

                write options.columnSeparatorStart

                for ci = 0 to columnCount - 1 do
                    let column = row.[ci]
                    let columnWidth = columnSizes.[ci]
                    let alignment = getColumnAlignment ci options

                    let text =
                        match alignment with
                        | ColumnTextAlignment.Center -> column |> String.padCenterWith ' ' columnWidth
                        | ColumnTextAlignment.Right -> column |> String.padLeftWith ' ' columnWidth
                        | _ -> column |> String.padRightWith ' ' columnWidth

                    write text

                    if ci < columnCount - 1 then
                        write options.columnSeparator

                write options.columnSeparatorEnd
                writeln ""
