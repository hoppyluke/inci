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

let private expect value =
  match value with
  | Some x -> x
  | None -> failwith "Value expected"

let private setupIncident (collection : InMemoryCollection) =
  declare "Test incident" (Events.nowish())
  |> collection.Put
  |> collection.Select

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
  let incident = setupIncident collection
  let result = whichCommand (collection.ToProvider()) true
  assertSuccess result

[<Fact>]
let ``resolve without incident raises error`` () =
  let collection = InMemoryCollection.Create()
  Assert.Throws<ValidationError>(fun () -> ignore(resolveCommand (collection.ToProvider()) [| "12:34" |])) 

let private isResolution e =
  e.Type = EventType.Declaration && e.IsResolved

[<Fact>]
let ``resolve succeeds with incident`` () =
  let collection = InMemoryCollection.Create()
  ignore (setupIncident collection)
  let result = resolveCommand (collection.ToProvider()) [| |]
  assertSuccess result
  let incident = expect (collection.Current())
  Assert.Contains(incident.Events, isResolution)

let private assertHasTime eventSelector startTime incident =
  let foundEvent = List.find eventSelector incident.Events
  Assert.False(foundEvent.Time.IsPrecise)
  Assert.True(foundEvent.Time.Timestamp > startTime)

[<Fact>]
let ``resolve defaults time`` () =
  let start = DateTimeOffset.Now
  let collection = InMemoryCollection.Create()
  ignore (setupIncident collection)
  let result = resolveCommand (collection.ToProvider()) [| |]
  assertSuccess result
  let incident = expect (collection.Current())
  assertHasTime isResolution start incident

[<Fact>]
let ``resolve sets time`` () =
  let start = DateTimeOffset.Now
  let collection = InMemoryCollection.Create()
  ignore (setupIncident collection)
  let result = resolveCommand (collection.ToProvider()) [| "2030-02-03T08:00" |]
  assertSuccess result
  let incident = expect (collection.Current())
  let resolution = List.find isResolution incident.Events
  Assert.True(resolution.Time.IsPrecise)
  Assert.Equal(2030, resolution.Time.Timestamp.Year)
  Assert.Equal(2, resolution.Time.Timestamp.Month)
  Assert.Equal(3, resolution.Time.Timestamp.Day)