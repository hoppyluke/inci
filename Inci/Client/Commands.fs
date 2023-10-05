module Inci.Client.Commands

open Inci.Core
open Inci.Core.Events
open Inci.Core.IncidentManagement
open Inci.Client.IO

type Result =
  | Success of string
  | Error of string

let private succeedWith s =
  Success(s)

let private failWith s =
  Failure(s)

exception ValidationError of string

let require name v =
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

let private incidentDetails i =
  $"{i.Id:n}: {i.Name}"

let declareCommand provider (args : string[]) =
  if args.Length < 1 then Error("usage: inci declare name [time]")
  else
    try
      declare args[0] (ensureTime (argValue 1 args))
      |> provider.Put
      |> provider.Select
      |> incidentDetails
      |> succeedWith
    with
      | ValidationError(m) -> Error m
