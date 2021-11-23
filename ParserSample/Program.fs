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

type IrisRepo() =
    member this.Clone() = ()

type IrisCLI() =
    class
    end


open Metadata

module DotNet =
    module New =
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

        let execute context =
            async {
                writelnC green "RUNNING DOTNET NEW"
                return { exitCode = 1 }
            }

        let verb =
            verb "new"
            |> withExecute execute
            |> withGroup (
                group
                |> withGroupSummary "Create a new .NET project or file."
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
            )

    let diagnosticsFlag =
        flag "diagnostics" 'd' "Enable diagnostic output."

    let infoFlag =
        longFlag "info" "Display .NET information."

    let listRuntimesFlag =
        longFlag "list-runtimes" "Display the installed runtimes."

    let listSdksFlag =
        longFlag "list-sdks" "Display the installed SDKs."

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
        |> withHelpFlag
        |> withDetailedHelpFlag
        |> withVersionFlag
        |> withVerb New.verb

    let verb =
        verb "dotnet"
        |> withGroup runtimeGroup
        |> withGroup sdkGroup

    let metadata = create "6.0.100" verb

[<EntryPoint>]
let main argv =
    let services =
        let builder = ServiceCollection()

        builder.AddSingleton<ICommandLineTokenizer, CommandLineTokenizer>()
        |> ignore

        builder.AddSingleton<ICommandLineMetadataProvider>
            (fun svp -> StaticCommandLineMetadataProvider(DotNet.metadata) :> ICommandLineMetadataProvider)
        |> ignore

        builder.AddSingleton<ICommandLineRuntime, CommandLineMetadataRuntime>()
        |> ignore

        builder.BuildServiceProvider()

    let clr =
        services.GetRequiredService<ICommandLineRuntime>()

    let context = { args = argv |> List.ofArray }

    let execution = clr.Execute context

    let result = execution |> Async.RunSynchronously

    result.exitCode
