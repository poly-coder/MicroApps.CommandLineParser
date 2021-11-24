open System
open Microsoft.Extensions.DependencyInjection
open MicroApps.CommandLineParser
open Console

type RollForwardSetting =
    | LatestPatch = 1
    | Minor = 2
    | LatestMinor = 3
    | Major = 4
    | LatestMajor = 5
    | Disable = 6

open Metadata

module DotNet =

    module RuntimeGroup =
        let additionalprobingpathOption =
            longOption "additionalprobingpath" stringReader "Path containing probing policy and assemblies to probe for."

        let additionalDepsOption =
            longOption "additional-deps" stringReader "Path to additional deps.json file."
        
        let depsfileOption =
            longOption "depsfile" stringReader "Path to <application>.deps.json file."
        
        let fxVersionOption =
            longOption "fx-version" stringReader "Version of the installed Shared Framework to use to run the application."

        let runtimeGroup =
            group
            |> withUsage "[runtime-options] [path-to-application] [arguments]"
            |> withDefaultUsage
            |> withGroupName "DotNet Runtime"
            |> withGroupSummary "Execute a .NET application."
            |> withOption additionalprobingpathOption
            |> withOption additionalDepsOption
            |> withOption depsfileOption
            |> withOption fxVersionOption

    module SdkGroup =
        let diagnosticsFlag =
            flag "diagnostics" 'd' "Enable diagnostic output."

        let infoFlag =
            longFlag "info" "Display .NET information."

        let listRuntimesFlag =
            longFlag "list-runtimes" "Display the installed runtimes."

        let listSdksFlag =
            longFlag "list-sdks" "Display the installed SDKs."

        module NewVerb =
            let listFlag =
                flag
                    "list"
                    'l'
                    "Lists templates containing the specified template name. If no name is specified, lists all templates."

            let interactiveFlag =
                longFlag
                    "interactive"
                    "Allows the internal dotnet restore command to stop and wait for user input or action (for example to complete authentication)."

            let dryRunFlag =
                longFlag
                    "dry-run"
                    "Displays a summary of what would happen if the given command line were run if it would result in a template creation."

            let forceFlag =
                longFlag "force" "Forces content to be generated even if it would change existing files."

            let nameOption =
                option
                    "name"
                    'n'
                    stringReader
                    "The name for the output being created. If no name is specified, the name of the output directory is used."

            let outputOption =
                option "output" 'o' (namedStringReader "folder") "Location to place the generated output."

            let installOption =
                option "install" 'i' (namedStringReader "folder|nuget-id") "Installs a source or a template package."

            let uninstallOption =
                option "uninstall" 'u' (namedStringReader "folder|nuget-id") "Uninstalls a source or a template package."

            let addSourceOption =
                longOption "add-source" (namedStringReader "source") "Specifies a NuGet source to use during install."
                |> withLongOption "nuget-source"

            let typeOption =
                longOption
                    "type"
                    (namedStringReader "type")
                    """Filters templates based on available types. Predefined values are "project" and "item"."""

            let langOption =
                longOption
                    "language"
                    (namedStringReader "lang")
                    "Filters templates based on language and specifies the language of the template to create."
                |> withLongOption "lang"

            let execute (context: ExecuteVerbContext) =
                async {
                    writelnC green "RUNNING DOTNET NEW"
                    context.valueMap
                    |> Seq.iter (fun pair ->
                        writelnC cyan $"  %s{pair.Key} => %O{pair.Value}")
                    return { exitCode = 0 }
                }

            let defaultGroup =
                group
                |> withGroupSummary "Create a new .NET project or file."
                |> withDefaultUsage
                |> withFlag listFlag
                |> withFlag interactiveFlag
                |> withFlag dryRunFlag
                |> withFlag forceFlag
                |> withOption nameOption
                |> withOption outputOption
                |> withOption installOption
                |> withOption uninstallOption
                |> withOption addSourceOption
                |> withOption typeOption
                |> withOption langOption

            let verb =
                verb "new"
                |> withExecute execute
                |> withGroup defaultGroup

        let sdkGroup =
            group
            |> withUsage "[sdk-options] [command] [command-options] [arguments]"
            |> withDefaultUsage
            |> withGroupName "DotNet SDK"
            |> withGroupSummary "Execute a .NET SDK command."
            |> withFlag diagnosticsFlag
            |> withFlag infoFlag
            |> withFlag listRuntimesFlag
            |> withFlag listSdksFlag
            |> withVerb NewVerb.verb

    let globalGroup =
        group
        |> withGroupName "Global"
        |> hideSummary
        |> withHelpFlag
        |> withDetailedHelpFlag
        |> withVersionFlag

    let verb =
        verb "dotnet"
        |> withGroup RuntimeGroup.runtimeGroup
        |> withGroup SdkGroup.sdkGroup
        |> withGroup globalGroup

    let metadata = create "6.0.100" verb

module DotNetRefl =
    [<Name("DotNet Runtime")>]
    [<Summary("Execute a .NET application.")>]
    [<DefaultUsage>]
    [<Usage("[runtime-options] [path-to-application] [arguments]")>]
    type RuntimeOptions = {
        [<Summary("Path containing probing policy and assemblies to probe for.")>]
        [<TypeName("path")>]
        additionalProbingPath: string list

        [<Summary("Path to additional deps.json file.")>]
        [<TypeName("path")>]
        additionalDeps: string list

        [<Summary("Path to <application>.deps.json file.")>]
        [<TypeName("path")>]
        depsFile: string list

        [<Summary("Version of the installed Shared Framework to use to run the application.")>]
        [<TypeName("version")>]
        fxVersion: string list
    }

    [<Name("DotNet SDK")>]
    [<Summary("Execute a .NET SDK command.")>]
    [<DefaultUsage>]
    [<Usage("[sdk-options] [command] [command-options] [arguments]")>]
    type SdkOptions = {
        [<Summary("Enable diagnostic output.")>]
        [<AlternativeShortForms('d')>]
        diagnostics: bool

        [<Summary("Display .NET information.")>]
        info: bool

        [<Summary("Display the installed runtimes.")>]
        listRuntimes: bool

        [<Summary("Display the installed SDKs.")>]
        listSdks: bool
    }
    module NewVerb =
    
        [<Name("DotNet SDK")>]
        [<Summary("Execute a .NET SDK command.")>]
        [<DefaultUsage>]
        [<Usage("[sdk-options] [command] [command-options] [arguments]")>]
        type NewOptions = {
            [<Summary("Lists templates containing the specified template name. If no name is specified, lists all templates.")>]
            [<AlternativeShortForms('l')>]
            list: bool

            [<Summary("Allows the internal dotnet restore command to stop and wait for user input or action (for example to complete authentication).")>]
            interactive: bool

            [<Summary("Displays a summary of what would happen if the given command line were run if it would result in a template creation.")>]
            dryRun: bool

            [<Summary("Forces content to be generated even if it would change existing files.")>]
            force: bool

            [<Summary("The name for the output being created. If no name is specified, the name of the output directory is used.")>]
            [<AlternativeShortForms('n')>]
            name: string

            [<Summary("Location to place the generated output.")>]
            [<AlternativeShortForms('o')>]
            [<TypeName("folder")>]
            output: string

            [<Summary("Installs a source or a template package.")>]
            [<AlternativeShortForms('i')>]
            [<TypeName("folder|nuget-id")>]
            install: string

            [<Summary("Installs a source or a template package.")>]
            [<AlternativeShortForms('u')>]
            [<TypeName("folder|nuget-id")>]
            uninstall: string

            [<Summary("Specifies a NuGet source to use during install.")>]
            [<AlternativeLongForms("nuget-source")>]
            [<TypeName("source")>]
            addSource: string list

            [<Summary("""Filters templates based on available types. Predefined values are "project" and "item".""")>]
            [<LongForm("type")>]
            [<TypeName("type")>]
            type': string

            [<Summary("Filters templates based on language and specifies the language of the template to create.")>]
            [<AlternativeLongForms("lang")>]
            [<TypeName("lang")>]
            language: string
        }

        [<OptionKey("new")>]
        type NewVerb(
            [<OptionGroup>] options: NewOptions) =

            interface ICommandLineExecute with
                member this.Execute() = async {
                    writelnC cyan "dotnet new executed"
                    return 1
                }

    [<OptionKey("dotnet")>]
    [<SubCommands(typeof<NewVerb.NewOptions>)>]
    type DotNetCommand(
        [<OptionGroup>] runtimeOptions: RuntimeOptions,
        [<OptionGroup>] sdkOptions: SdkOptions
        ) =

        interface ICommandLineExecute with
                member this.Execute() = async {
                    writelnC cyan "dotnet executed"
                    return 0
                }

[<EntryPoint>]
let main argv =
    let services =
        let builder = ServiceCollection()

        builder.AddSingleton<ICommandLineTokenizer, CommandLineTokenizer>()
        |> ignore

        builder.AddSingleton<ICommandLineMetadataProvider>
            (fun svp ->
                ReflectionMetadataProvider(
                    typeof<DotNetRefl.DotNetCommand>,
                    "6.0.100")
                :> ICommandLineMetadataProvider)
        |> ignore

        //builder.AddSingleton<ICommandLineMetadataProvider>
        //    (fun svp -> StaticCommandLineMetadataProvider(DotNet.metadata) :> ICommandLineMetadataProvider)
        //|> ignore

        builder.AddSingleton<ICommandLineRuntime, CommandLineMetadataRuntime>()
        |> ignore

        builder.BuildServiceProvider()

    let clr =
        services.GetRequiredService<ICommandLineRuntime>()

    let context = { args = argv |> List.ofArray }

    let execution = clr.Execute context

    let result = execution |> Async.RunSynchronously

    result.exitCode
