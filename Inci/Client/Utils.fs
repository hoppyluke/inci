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

let canonicalName (name : string) =
  name.Trim().ToLowerInvariant()
  |> expand

let private commands = Map [
    ("declare", [])
    ("resolve", [])
    ("rename", [])
    ("switch", [])
    ("list", [])
    ("timeline", [])
    ("alert", ["fired"; "resolved"])
    ("monitor", ["down"; "up"])
    ("observation", ["add"])
    ("action", ["add"])
]

