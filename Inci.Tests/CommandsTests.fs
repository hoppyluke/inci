module Inci.Client.CommandsTests

open System
open Xunit
open Inci.Core
open Inci.Core.IncidentManagement
open Inci.Client.Commands
open Inci.Client.IO

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

      member this.Select(incident) =
        this.Selected <- Some incident.Id
        incident
    
      member this.ToProvider() =
        { Current = this.Current; Put = this.Put; Switch = this.Switch; Select = this.Select}

      static member Create() =
        { Selected = None; Incidents = Map.empty }

let private assertError r =
  match r with
  | Success m -> Assert.Fail("Expected error result but got success: " + m)
  | _ -> ()

let private assertSuccess r =
  match r with
  | Error e -> Assert.Fail("Expected success result but got error: " + e)
  | _ -> ()

[<Fact>]
let ``declare errors with no args`` () =
  let result = declareCommand (InMemoryCollection.Create().ToProvider()) Array.empty
  assertError result

[<Fact>]
let ``declare succeeds without time`` () =
  let args = [| "test name" |]
  let result = declareCommand (InMemoryCollection.Create().ToProvider()) args
  assertSuccess result

[<Fact>]
let ``declare errors with invalid time`` () =
  let args = [| "test name"; "foo" |]
  Assert.Throws<ValidationError>(fun () -> ignore(declareCommand (InMemoryCollection.Create().ToProvider()) args))

[<Fact>]
let ``declare succeeds with valid time`` () =
  let args = [| "test name"; "12:34" |]
  let result = declareCommand (InMemoryCollection.Create().ToProvider()) args
  assertSuccess result

[<Fact>]
let ``declare sets current incident`` () =
  let args = [| "test name" |]
  let collection = InMemoryCollection.Create()
  let result = declareCommand (collection.ToProvider()) args
  assertSuccess result
  Assert.True(collection.Current().IsSome)

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``which without incident respects error option`` (shouldFail : bool) =
  let collection = InMemoryCollection.Create()
  let result = whichCommand (collection.ToProvider()) shouldFail
  match result with
  | Error e -> Assert.Equal("No current incident", e)
               Assert.True(shouldFail, "Error result returned when success was expected")
  | Success s -> Assert.Equal("No current incident", s)
                 Assert.False(shouldFail, "Success result returned when failure was expected")

[<Fact>]
let ``which returns current incident details`` () =
  let collection = InMemoryCollection.Create()
  let incident = declare "Test" (Events.nowish())
                 |> collection.Put
                 |> collection.Select
  let result = whichCommand (collection.ToProvider()) true
  assertSuccess result

[<Fact>]
let ``resolve without incident raises error`` () =
  let collection = InMemoryCollection.Create()
  Assert.Throws<ValidationError>(fun () -> ignore(resolveCommand (collection.ToProvider()) [| "12:34" |])) 
  
[<Fact>]
let ``resolve succeeds with incident`` () =
  let collection = InMemoryCollection.Create()
  let incident = declare "Test" (Events.nowish())
                 |> collection.Put
                 |> collection.Select
  let result = resolveCommand (collection.ToProvider()) [| |]
  assertSuccess result
  let updated = collection.Current()
  match updated with
  | Some i -> Assert.Contains(i.Events, fun e -> e.IsResolved && e.Type = EventType.Declaration)
  | None -> Assert.Fail("No incident")