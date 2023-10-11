module Inci.Client.IO

open Inci.Core
open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

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

type private EventTypeConverter() =
    inherit JsonConverter<EventType>()
    override c.Read(reader, typeToConvert, options) =
        let token = reader.GetString()
        match token with
        | "Observation" -> Observation
        | "Action" -> EventType.Action
        | "Monitor" -> Monitor
        | "Alert" -> Alert
        | "Declaration" -> Declaration
        | _ -> raise (JsonException(sprintf "Invalid event type '%s'" token))
    
    override c.Write(writer, value, options) =
        let token = match value with
                    | Observation -> "Observation"
                    | Action -> "Action"
                    | Monitor -> "Monitor"
                    | Alert -> "Alert"
                    | Declaration -> "Declaration"
        writer.WriteStringValue(token)

let private jsonOptions = JsonSerializerOptions()
jsonOptions.Converters.Add(new EventTypeConverter())

let private ensureDirectory path =
    if not (Path.Exists(path)) then
        ignore (Directory.CreateDirectory(path))
 
let put (inci : Incident) =
    ensureDirectory docs
    ensureDirectory data
    use file = File.Create(fileName inci)
    JsonSerializer.Serialize(file, inci, jsonOptions)
    use current = File.CreateText(currentIncidentFile)
    current.Write(inci.Id.ToString("n"))
    inci

let private read id =
    let f = fileNameById id
    if Path.Exists(f) then
        use file = File.OpenRead(f)
        Some(JsonSerializer.Deserialize<Incident>(file, jsonOptions))
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