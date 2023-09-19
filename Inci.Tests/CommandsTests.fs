module Inci.Core.CommandsTests

open System
open Xunit
open Inci.Core.Commands
open Inci.Core.Events

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

[<Fact>]
let ``declare creates new incident`` () =
  let p = inMemoryProvider (InMemoryCollection.Create())
  let name = "Test"
  let result = declare p name (Some(now()))
  Assert.True(result.IsSome)
  Assert.Equal(name, result.Value.Name)

[<Fact>]
let ``declare sets current incident`` () =
  let p = inMemoryProvider (InMemoryCollection.Create())
  let result = declare p "Test" (Some(now()))
  Assert.True(result.IsSome)
  Assert.Equal(result, p.Current())

[<Fact>]
let ``declare defaults time to nowish`` () =
  let p = inMemoryProvider (InMemoryCollection.Create())
  let result = declare p "Test" None
  Assert.True(result.IsSome)
  let declaration = List.find (fun e -> e.Type = Declaration) result.Value.Events
  Assert.False(declaration.Time.IsPrecise)

[<Fact>]
let ``resolve without incident returns none`` () =
  let p = inMemoryProvider (InMemoryCollection.Create())
  let result = resolve p (Some(now()))
  Assert.True(result.IsNone)

[<Fact>]
let ``resolve adds resolution`` () =
  let p = inMemoryProvider (InMemoryCollection.Create())
  let i = (IncidentManagement.declare "Test" (now())) |> p.Put
  p.Switch i.Id |> ignore
  let result = resolve p (Some(now()))
  Assert.True(result.IsSome)
  let resolution = List.find (fun e -> e.Type = Declaration && e.IsResolved = true) result.Value.Events
  Assert.True(resolution.Time.IsPrecise)