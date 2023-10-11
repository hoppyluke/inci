open Inci.Client.Commands
open Inci.Client.Utils
open Inci.Client.IO

let version = "0.0.1"

let private usageMessage = "usage: inci command [args]"

let private dispatch (args : string[]) =
    let command = canonicalName args[0]
    match command with
    | "version" -> Success(version)
    | "declare" -> declareCommand fileProvider args[1..]
    | _ -> Error(usageMessage)

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "%s" usageMessage
        0
    else
        let result = dispatch args
        match result with
        | Success msg -> printfn "%s" msg; 0
        | Error e -> eprintfn "%s" e; 1