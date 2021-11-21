namespace MicroApps.CommandLineParser

open System.Text.RegularExpressions

type ArgumentToken =
    | ShortFormToken of char
    | LongFormToken of string
    | ValueToken of string

type ICommandLineTokenizer =
    abstract Tokenize : string list -> Result<ArgumentToken list, string list>
    
type CommandLineTokenizer() =
    let longFormPattern = Regex(@"^\-\-(?<value>\w+(\-\w+)*)$")
    let shortFormPattern = Regex(@"^\-(?<value>\w+)$")

    let (|IsLongForm|_|) arg =
        if isNull arg then None else
        let m = longFormPattern.Match arg
        if not m.Success then None else
        m.Groups.["value"].Value |> Some

    let (|IsShortForm|_|) arg =
        if isNull arg then None else
        let m = shortFormPattern.Match arg
        if not m.Success then None else
        m.Groups.["value"].Value.ToCharArray() |> List.ofArray |> Some

    interface ICommandLineTokenizer with
        member this.Tokenize args = 
            let rec loop args tokens errors =
                match args with
                | [] ->
                    match errors with
                    | [] -> Ok (tokens |> List.rev)
                    | errors -> Error (errors |> List.rev)
                | head :: tail ->
                    match head with
                    | IsLongForm value ->
                        let token = LongFormToken value
                        loop tail (token :: tokens) errors
                    | IsShortForm chars ->
                        let newTokens =
                            chars
                            |> List.map ShortFormToken
                            |> List.rev
                        loop tail (newTokens @ tokens) errors
                    | value ->
                        let token = ValueToken value
                        loop tail (token :: tokens) errors
                    
            loop args [] []
