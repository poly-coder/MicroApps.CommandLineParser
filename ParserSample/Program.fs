open System
open Microsoft.Extensions.DependencyInjection
open MicroApps.CommandLineParser

type RollForwardSetting =
    | LatestPatch = 1
    | Minor = 2
    | LatestMinor = 3
    | Major = 4
    | LatestMajor = 5
    | Disable = 6

let sampleMetadata =
    let newArg =
        let desc = Metadata.summarized "Create a new .NET project or file."
        let verb =
            Metadata.verbArg "new"
            |> Metadata.withOptionArg "list" 'l' 
                "Lists templates containing the specified template name. If no name is specified, lists all templates."
                Metadata.stringReader
            |> Metadata.withOptionArg "name" 'n' 
                "The name for the output being created. If no name is specified, the name of the output directory is used."
                Metadata.stringReader
            |> Metadata.withOptionArg "output" 'o' 
                "Location to place the generated output."
                (Metadata.namedStringReader "folder")
            |> Metadata.withOptionArg "install" 'i' 
                "Installs a source or a template package."
                Metadata.stringReader
            |> Metadata.withOptionArg "uninstall" 'u' 
                "Uninstalls a source or a template package."
                Metadata.stringReader
            |> Metadata.withLongFlagArg "interactive"
                "Allows the internal dotnet restore command to stop and wait for user input or action (for example to complete authentication)."
            |> Metadata.withArg (
                    let desc = Metadata.summarized "Specifies a NuGet source to use during install."
                    let arg = Metadata.optional "add-source"
                    let opt =
                        Metadata.optionArg "add-source" (Metadata.namedStringReader "source")
                        |> Metadata.withOptionMultiValued Metadata.multiValued
                    CommandLineOption(desc, arg, opt)
                )
        CommandLineVerb(desc, verb)

    let dotnetDesc, dotnetVerb =
        let desc =
            Metadata.named ".NET SDK (6.0.100)"
            |> Metadata.withSummary "Execute a .NET application."
            |> Metadata.withDescription "Execute a .NET application."

        let verb =
            Metadata.defaultVerbArg "dotnet"
            |> Metadata.withUsage "dotnet [path-to-application]"
            |> Metadata.withOptionalPositionalArg 1 "path-to-application"
                "The path to an application .dll file to execute."
                Metadata.existingFilePathReader
            |> Metadata.withLongOptionMultiArg "additionalprobingpath"
                "Path containing probing policy and assemblies to probe for."
                Metadata.stringReader
            |> Metadata.withLongOptionMultiArg "additional-deps"
                "Path to additional deps.json file."
                Metadata.stringReader
            |> Metadata.withLongOptionArg "depsfile"
                "Path to <application>.deps.json file."
                Metadata.existingFilePathReader
            |> Metadata.withLongOptionArg "fx-version"
                "Path to <application>.deps.json file."
                (Metadata.namedStringReader "version")
            |> Metadata.withLongOptionArg "roll-forward"
                "Roll forward to framework version  (LatestPatch, Minor, LatestMinor, Major, LatestMajor, Disable)."
                (Metadata.enumReader<RollForwardSetting> "setting")
            |> Metadata.withLongOptionArg "runtimeconfig"
                "Path to <application>.runtimeconfig.json file."
                Metadata.existingFilePathReader
            |> Metadata.withFlagArg "diagnostics" 'd' "Enable diagnostic output."
            |> Metadata.withLongFlagArg "info" "Display .NET information."
            |> Metadata.withLongFlagArg "list-runtimes" "Display the installed runtimes."
            |> Metadata.withLongFlagArg "list-sdks" "Display the installed SDKs."
            |> Metadata.withHelpArg
            |> Metadata.withVersionArg
            |> Metadata.withExamplesArg
            |> Metadata.withArg newArg

        desc, verb

    Metadata.create dotnetDesc dotnetVerb

[<EntryPoint>]
let main argv =
    let services =
        let builder = ServiceCollection()

        builder.AddSingleton<ICommandLineTokenizer, CommandLineTokenizer>()
        |> ignore

        builder.AddSingleton<ICommandLineMetadataProvider>
            (fun svp -> StaticCommandLineMetadataProvider(sampleMetadata) :> ICommandLineMetadataProvider)
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
