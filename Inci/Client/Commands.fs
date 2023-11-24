module Inci.Client.Commands

open Inci.Core
open Inci.Core.Events
open Inci.Core.IncidentManagement
open Inci.Client.IO
open Inci.Client.Utils

type Result =
  | Success of string
  | Error of string

type Command = Provider -> string[] -> Result
type CommandGroup = string * Map<string, Command>

let maybeResult isError msg =
  match isError with
  | true -> Error msg
  | false -> Success msg

exception ValidationError of string

let private require name v =
  match v with
  | Some x -> x
  | None -> raise (ValidationError($"{name} is invalid"))

let private ensureTime time =
  match time with
  | Some s -> require "time" (parse s)
  | None -> nowish()

let private argValue index (args : string[]) =
  if index < args.Length then Some(args[index])
  else None 

let private ensureIncident provider =
  match provider.Current() with
  | Some i -> i
  | None -> raise (ValidationError "No current incident")

let private updateIncident provider action =
  ensureIncident provider
  |> action
  |> provider.Put

let private incidentResult incident =
  Success $"{incident.Id:n}: {incident.Name}"

let declareCommand provider (args : string[]) =
  if args.Length < 1 then Error("usage: inci declare <name> [time]")
  else
    declare args[0] (ensureTime (argValue 1 args))
    |> provider.Put
    |> provider.Select
    |> incidentResult

let whichCommand provider noneIsError =
  match provider.Current() with
  | None -> maybeResult noneIsError "No current incident"
  | Some i -> incidentResult i

let resolveCommand provider (args : string[]) =
  let time = ensureTime (argValue 0 args)
  updateIncident provider (resolve time)
  |> fun i -> Success (sprintf "%s resolved at %s" i.Name (formatTime time))

let private eventVerb (event : Inci.Core.Event) =
  match (event.Type, event.IsResolved) with
  | Monitor, false -> "down"
  | Monitor, true -> "up"
  | Alert, false -> "fired"
  | Alert, true -> "resolved"
  | _, _ -> ""

let private formatDescription event =
  match event.Type with
  | Monitor | Alert -> event.Description + " " + eventVerb event
  | _ -> event.Description

let private eventDetails (event : Inci.Core.Event) =
  $"{event.Id}: {formatDescription event} @ {(formatTime event.Time)}"

let private lastEvent incident =
  incident.Events.Head

let private eventResult = lastEvent >> eventDetails >> Success

let handler (group : CommandGroup) provider (args : string[]) =
  let (name, commandMap) = group
  if args.Length < 1 then raise (ValidationError($"usage: inci {name} <action> [args]"))
  match Map.tryFind (canonicalise(args[0])) commandMap with
  | Some cmd -> cmd provider args[1..]
  | None -> raise (ValidationError($"unknown command: {name} {args[0]}"))

let private listEvents eventType provider (args : string[]) =
  ensureIncident provider
  |> list eventType
  |> List.map eventDetails
  |> String.concat System.Environment.NewLine
  |> Success

let private addEvent operation noun verb provider (args : string[]) =
  if args.Length < 1 then Error $"usage: inci {noun} {verb} <description> [time]"
  else
    let time = ensureTime (argValue 1 args)
    updateIncident provider (operation time args[0])
    |> eventResult

let private addResolution operation noun verb provider (args : string[]) =
  if args.Length < 1 then Error $"usage: inci {noun} {verb} <id> [time]"
  else
    let time = ensureTime (argValue 1 args)
    let incident = ensureIncident provider
                   |> operation time args[0]
    match incident with
    | Some i -> provider.Put i |> eventResult
    | None -> Error $"invalid {noun} ID: {args[0]}"

let observationCommands = ("observation", Map [
  ("list", listEvents Observation)
  ("add", addEvent observed "observation" "add")
])

let actionCommands = ("action", Map [
  ("add", addEvent acted "action" "add")
  ("list", listEvents Action)
])

let alertCommands = ("alert", Map [
  ("fired", addEvent alertFired "alert" "fired")
  ("resolved", addResolution alertResolved "alert" "fired")
  ("list", listEvents Alert)
])

let monitorCommands = ("monitor", Map [
  ("down", addEvent down "monitor" "down")
  ("up", addResolution up "monitor" "up")
  ("list", listEvents Monitor)
])

let private maxLength (projection: 'T -> string) list =
  List.map projection list
  |> List.map (fun s -> s.Length)
  |> List.max

let private emoji event =
  match (event.Type, event.IsResolved) with
  | Observation, _ -> "🔍"
  | Action, _ -> "🔧"
  | Monitor, false -> "🔴"
  | Monitor, true -> "🟢"
  | Alert, false -> "🔶"
  | Alert, true -> "🔷"
  | Declaration, false -> "🔥"
  | Declaration, true -> "🏁"

let private prettyPrint idSize timeSize (event : Event) =
  let id = event.Id.PadLeft(idSize)
  let t = (formatTime event.Time).PadRight(timeSize)
  $"{t} {emoji event} {id} {formatDescription event}"

let timelineCommand provider (args : string[]) =
  let incident = ensureIncident provider
  let events = List.sort incident.Events
  let idSize = maxLength (fun (e : Event) -> e.Id) events
  let timeSize = maxLength (fun e -> formatTime e.Time) events
  List.map (prettyPrint idSize timeSize) events
  |> String.concat System.Environment.NewLine
  |> Success