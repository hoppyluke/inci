module Inci.Core.Commands

open System
open Inci.Core.Events

type Provider =
    { Current: unit -> Incident option
      Switch: Guid -> Incident option
      Index: unit -> Incident seq
      Put: Incident -> Incident }

let private ensureTime time =
  match time with
  | Some t -> t
  | None -> nowish()

let declare provider name start =
  IncidentManagement.declare name (ensureTime start)
  |> provider.Put
  |> fun i -> i.Id
  |> provider.Switch

let switch provider id =
  provider.Switch id