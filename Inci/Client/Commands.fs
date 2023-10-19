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

let private incidentDetails i =
  $"{i.Id:n}: {i.Name}"

let declareCommand provider (args : string[]) =
  if args.Length < 1 then Error("usage: inci declare <name> [time]")
  else
    declare args[0] (ensureTime (argValue 1 args))
    |> provider.Put
    |> provider.Select
    |> incidentDetails
    |> Success

let whichCommand provider noneIsError =
  match provider.Current() with
  | None -> maybeResult noneIsError "No current incident"
  | Some i -> Success (incidentDetails i)

let resolveCommand provider (args : string[]) =
  let time = ensureTime (argValue 0 args)
  resolve time (ensureIncident provider)
  |> provider.Put
  |> fun i -> sprintf "%s resolved at %s" i.Name (formatTime time)
  |> Success 

let private eventDetails (event : Inci.Core.Event) =
  sprintf "%s: %s" event.Id event.Description

let private listObservationsCommand provider (args : string[]) =
  ensureIncident provider
  |> list Observation
  |> List.map eventDetails
  |> String.concat System.Environment.NewLine
  |> Success

let observationCommands = ("observation", Map [
  ("list", listObservationsCommand)
])

let handler (group : CommandGroup) provider (args : string[]) =
  let (name, commandMap) = group
  if args.Length < 1 then raise (ValidationError($"usage: inci {name} <action> [args]"))
  match Map.tryFind (canonicalise(args[0])) commandMap with
  | Some cmd -> cmd provider args
  | None -> raise (ValidationError($"unknown command: {name} {args[0]}"))

