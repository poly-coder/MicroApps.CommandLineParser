namespace MicroApps.CommandLineParser

type CommandLineExecuteContext = {
    args: string list
}

type CommandLineExecuteResult = {
    exitCode: int
}

type ICommandLineRuntime =
    abstract Execute: CommandLineExecuteContext -> Async<CommandLineExecuteResult>
