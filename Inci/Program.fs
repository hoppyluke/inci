open Inci.Client.Utils

let version = "0.0.1"

let private printUsage () =
    printfn "inci %s" version
    ()

// let private dispatchCommand args =
//     let command = canonicalName args[0]
//     let 

[<EntryPoint>]
let main args =
    if args.Length = 0 then printUsage()
    else
        let command = canonicalName args[0]
        match command with
        | "declare" -> ()
        | _ -> ()

    0