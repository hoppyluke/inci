module Inci.Core.CommandsTests

open System
open Xunit
open Inci.Core.Commands

type InMemoryCollection =
    { mutable Selected: Guid option
      mutable Incidents: Map<Guid, Incident> }

      member this.Index() =
        seq this.Incidents.Values

      member this.Current() =
        match this.Selected with
        | Some id ->  Some this.Incidents[id]
        | None -> None
    
      member this.Put(i) =
        this.Incidents <- Map.add i.Id i this.Incidents
        i
      
      member this.Switch(id) =
        match Map.tryFind id this.Incidents with
        | Some i -> this.Selected <- Some id
                    Some i
        | None -> None
    
      static member Create() =
        { Selected = None; Incidents = Map.empty }

let inMemoryProvider (collection : InMemoryCollection) =
    { Current = collection.Current; Switch = collection.Switch; Index = collection.Index; Put = collection.Put }

[<Fact>]
let ```switch to invalid ID is none`` () =
    let p = inMemoryProvider (InMemoryCollection.Create())
    let result = switch p (Guid.NewGuid())
    Assert.False(result.IsSome)
