module Inci.Core.IncidentManagement

open System

let private addEvent e i =
    { i with Events = e :: i.Events }

let declare name start =
    { Id = Guid.NewGuid()
      Name = name 
      Events = [{ Id = "d0"
                  Type = Declaration
                  Time = start
                  Description = "Incident declared"
                  IsResolved = false
               }]
      }

let resolve time i =
    addEvent { Id = "d1"; Type = Declaration; Time = time; Description = "Incident resolved"; IsResolved = true} i

let rename name i =
    { i with Name = name }

let private idPrefix t =
    match t with
    | Declaration -> "d"
    | Observation -> "o"
    | Alert -> "a"
    | Monitor -> "m"
    | Action -> "k"

let private idAfter prefix (e: Event) =
    let n = int (e.Id.Replace(prefix, ""))
    $"{prefix}%d{n + 1}"

let private nextId t i =
    let prefix = idPrefix t
    let events = List.filter (fun e -> e.Type = t) i.Events
    match events with
    | [] -> prefix + "0"
    | [_] -> prefix + "1"
    | _ -> List.sortByDescending (fun e -> e.Time.Timestamp) events |> List.head |> idAfter prefix

let observed time s i =
    let id = nextId Observation i
    addEvent { Id = id; Type = Observation; Time = time; Description = s; IsResolved = false} i