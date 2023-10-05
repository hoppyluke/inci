module Inci.Client.IO

open Inci.Core
open System
open System.IO
open System.Text.Json

type Provider =
    { Current: unit -> Incident option
      Switch: Guid -> Incident option
      Select: Incident -> Incident
      //Index: unit -> Incident seq
      Put: Incident -> Incident }

let private dir = ".inci"
let private docs = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), dir)
let private data = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), dir)
let private currentIncidentFile = Path.Combine(data, ".current")

let private fileName (i : Incident) =
    let f = i.Id.ToString("n") + ".json"
    Path.Combine(docs, f)

let private fileNameById (id : Guid) =
    Path.Combine(docs, id.ToString("n") + ".json")

let put (inci : Incident) =
    use file = File.Create(fileName inci)
    JsonSerializer.Serialize(file, inci)
    use current = File.CreateText(currentIncidentFile)
    current.Write(inci.Id.ToString("n"))
    inci

let private read id =
    let f = fileNameById id
    if Path.Exists(f) then
        use file = File.OpenRead(f)
        Some(JsonSerializer.Deserialize<Incident>(file))
    else None

let current () =
    if Path.Exists(currentIncidentFile) then
        let id = File.ReadAllText(currentIncidentFile)
        match Guid.TryParse id with
        | true, g -> read g
        | _ -> None
    else None

let select (incident : Incident) =
    File.WriteAllText(currentIncidentFile, incident.Id.ToString("n"))
    incident

let switch id =
    let i = read id
    match i with
    | None -> None
    | Some incident -> Some(select incident) 

let fileProvider = { Current = current; Put = put; Switch = switch; Select = select }