module Inci.Client.Utils

let private abbreviations = Map [
    ("alr", "alert")
    ("obs", "observation")
    ("mon", "monitor")
    ("act", "action")
    ("dec", "declare")
    ("res", "resolve")
    ("?", "which")
]

let private expand command =
    let abbreviation = Map.tryFind command abbreviations
    match abbreviation with
    | Some a -> a
    | None -> command

let canonicalise (name : string) =
    name.Trim().ToLowerInvariant()

let canonicalName (name : string) =
  expand (canonicalise name)

let private commands = Map [
    ("declare", [])
    ("resolve", [])
    ("rename", [])
    ("switch", [])
    ("list", [])
    ("timeline", [])
    ("alert", ["fired"; "resolved"; "list"])
    ("monitor", ["down"; "up"; "list"])
    ("observation", ["add"; "list"])
    ("action", ["add"; "list"])
]