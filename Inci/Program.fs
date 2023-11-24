open Inci.Client.Commands
open Inci.Client.Utils
open Inci.Client.IO
open System

let version = "0.0.1"

let private dispatch (args : string[]) =
    let command = canonicalName args[0]
    match command with
    | "version" -> Success(version)
    | "declare" -> declareCommand fileProvider args[1..]
    | "which" -> whichCommand fileProvider false
    | "resolve" -> resolveCommand fileProvider args[1..]
    | "observation" -> handler observationCommands fileProvider args[1..]
    | "action" -> handler actionCommands fileProvider args[1..]
    | "alert" -> handler alertCommands fileProvider args[1..]
    | "monitor" -> handler monitorCommands fileProvider args[1..]
    | "timeline" -> timelineCommand fileProvider args[1..]
    | _ -> Error($"{command} is not an inci command")

let private lf = Environment.NewLine

[<EntryPoint>]
let main args =
    Console.OutputEncoding <- System.Text.Encoding.UTF8
    if args.Length = 0 then
        printfn "usage: inci <command> [args]"
        let whichResult = whichCommand fileProvider true
        match whichResult with
        | Success s -> printfn "%sCurrent incident:%s%s%s" lf lf lf s
        | Error _ -> ()
        0
    else
        try
            let result = dispatch args
            match result with
            | Success msg -> printfn "%s" msg; 0
            | Error e -> eprintfn "%s" e; 1
        with
        | ValidationError m -> eprintfn "%s" m; 1