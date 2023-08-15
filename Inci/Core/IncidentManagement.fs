module Inci.Core.IncidentManagement

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

let private addEvent y t s r i =
    let id = nextId y i
    let e = { Id = id; Type = y; Time = t; Description = s; IsResolved = r}
    { i with Events = e :: i.Events }

let private resolutionOf time (event : Event) =
    { Id = event.Id; Type = event.Type; Time = time; Description = event.Description; IsResolved = true }

let private addResolution eventType id t i =
    let alert = List.tryFind (fun e -> e.Type = eventType && e.Id = id && not e.IsResolved) i.Events
    let resolution = List.tryFind (fun e -> e.Type = eventType && e.Id = id && e.IsResolved) i.Events
    match (alert, resolution) with
    | (Some _, Some _) -> Some i
    | (Some event, None) -> Some { i with Events = resolutionOf t event :: i.Events }
    | (None, _) -> None
                        

let declare name start =
    { Id = System.Guid.NewGuid()
      Name = name 
      Events = [{ Id = "d0"
                  Type = Declaration
                  Time = start
                  Description = "Incident declared"
                  IsResolved = false
               }]
      }

let resolve time i =
    addEvent Declaration time "Incident resolved" true i

let rename name i =
    { i with Name = name }

let observed time s i =
    addEvent Observation time s false i

let acted time s i =
    addEvent Action time s false i

let alertFired time s i =
    addEvent Alert time s false i

let alertResolved time id i =
    addResolution Alert id time i

let down time s i =
    addEvent Monitor time s false i

let up time id i =
    addResolution Monitor id time i