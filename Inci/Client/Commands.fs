module Inci.Client.Commands

open Inci.Core
open Inci.Core.Events
open Inci.Core.IncidentManagement
open Inci.Client.IO

type Result =
  | Success of string
  | Error of string

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
    try
      declare args[0] (ensureTime (argValue 1 args))
      |> provider.Put
      |> provider.Select
      |> incidentDetails
      |> Success
    with
      | ValidationError(m) -> Error m

let whichCommand provider noneIsError =
  match provider.Current() with
  | None -> maybeResult noneIsError "No current incident"
  | Some i -> Success (incidentDetails i)

let resolveCommand provider (args : string[]) =
  try
    let time = ensureTime (argValue 0 args)
    resolve time (ensureIncident provider)
    |> provider.Put
    |> fun i -> sprintf "%s resolved at %s" i.Name (formatTime time)
    |> Success 
  with
  | ValidationError m -> Error m