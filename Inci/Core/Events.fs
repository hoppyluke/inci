module Inci.Core.Events

open System
open System.Text.RegularExpressions

let at time =
    EventTime(time, true)
    
let about time =
    EventTime(time, false)

let now () =
    EventTime(DateTimeOffset.UtcNow, true)

let nowish () =
    EventTime(DateTimeOffset.UtcNow, false)

let private timeOnlyPattern = new Regex("^(\d{1,2}:\d{2}(?::\d{2}(?:.\d{1,3})?)?)(\??)$")

let private adjustDate timestamp =
    if timestamp > DateTimeOffset.UtcNow then timestamp.AddDays(-1)
    else timestamp

let private (|TimeOnly|_|) s =
    let m = timeOnlyPattern.Match(s)
    match m with
    | r when r.Success && r.Groups[2].Value = "?" -> Some(about(adjustDate(DateTimeOffset.Parse(r.Groups[1].Value))))
    | r when r.Success -> Some(at(adjustDate(DateTimeOffset.Parse(r.Groups[1].Value))))
    | _ -> None

let private (|ApproxDateAndTime|_|) (s: string) =
    if s.EndsWith("?") then match DateTimeOffset.TryParse(s.Substring(0, s.Length - 1)) with
                            | true, d -> Some(about d)
                            | _ -> None
    else None

let private (|DateAndTime|_|) (s : string) =
    match DateTimeOffset.TryParse s with
    | true, d -> Some(at d)
    | _ -> None

let parse t =
    match t with
    | null -> Some(about DateTimeOffset.UtcNow)
    | "" -> Some(about DateTimeOffset.UtcNow)
    | TimeOnly time -> Some time
    | ApproxDateAndTime d -> Some d
    | DateAndTime d -> Some d
    | _ -> None