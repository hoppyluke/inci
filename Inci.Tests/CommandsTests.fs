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

let private assertSuccessMessage messageAssertions r =
  match r with
  | Error e -> Assert.Fail("Expected success result but got error: " + e)
  | Success m -> messageAssertions m

let private expect value =
  match value with
  | Some x -> x
  | None -> failwith "Value expected"

let private setupWithoutIncident () =
  let start = DateTimeOffset.Now
  let collection = InMemoryCollection.Create()
  (start, collection.ToProvider())

let private setupWithIncident () =
  let start = DateTimeOffset.Now
  let collection = InMemoryCollection.Create()
  declare "Test incident" (Events.nowish())
  |> collection.Put
  |> collection.Select
  |> ignore
  (start, collection.ToProvider())

let private assertOnCurrentIncident provider result (assertions: Incident -> unit) =
  assertSuccess result
  let incident = expect (provider.Current())
  assertions incident

[<Fact>]
let ``declare errors with no args`` () =
  let _, provider = setupWithoutIncident()
  let result = declareCommand provider Array.empty
  assertError result

[<Fact>]
let ``declare succeeds without time`` () =
  let _, provider = setupWithoutIncident()
  let result = declareCommand provider [| "test name" |]
  assertOnCurrentIncident provider result (fun i -> Assert.Equal("test name", i.Name))

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
  let _, provider = setupWithoutIncident()
  let result = declareCommand provider [| "test name" |]
  assertOnCurrentIncident provider result ignore

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``which without incident respects error option`` (shouldFail : bool) =
  let _, provider = setupWithoutIncident()
  let result = whichCommand provider shouldFail
  match result with
  | Error e -> Assert.Equal("No current incident", e)
               Assert.True(shouldFail, "Error result returned when success was expected")
  | Success s -> Assert.Equal("No current incident", s)
                 Assert.False(shouldFail, "Success result returned when failure was expected")

[<Fact>]
let ``which returns current incident details`` () =
  let _, provider = setupWithIncident()
  let result = whichCommand provider true
  assertSuccess result

[<Fact>]
let ``resolve without incident raises error`` () =
  let _, provider = setupWithoutIncident()
  Assert.Throws<ValidationError>(fun () -> ignore(resolveCommand provider [| "12:34" |])) 

let private isResolution e =
  e.Type = EventType.Declaration && e.IsResolved

[<Fact>]
let ``resolve succeeds with incident`` () =
  let _, provider = setupWithIncident()
  let result = resolveCommand provider [| |]
  assertOnCurrentIncident provider result (fun i -> Assert.Contains(i.Events, isResolution))

let private assertHasTime eventSelector startTime incident =
  let foundEvent = List.find eventSelector incident.Events
  Assert.False(foundEvent.Time.IsPrecise)
  Assert.True(foundEvent.Time.Timestamp > startTime)

[<Fact>]
let ``resolve defaults time`` () =
  let start, provider = setupWithIncident()
  let result = resolveCommand provider [| |]
  assertOnCurrentIncident provider result (assertHasTime isResolution start)

[<Fact>]
let ``resolve sets time`` () =
  let _, provider = setupWithIncident()
  let result = resolveCommand provider [| "2030-02-03T08:00" |]
  let assertions incident =
    let resolution = List.find isResolution incident.Events
    Assert.True(resolution.Time.IsPrecise)
    Assert.Equal(2030, resolution.Time.Timestamp.Year)
    Assert.Equal(2, resolution.Time.Timestamp.Month)
    Assert.Equal(3, resolution.Time.Timestamp.Day)
  assertOnCurrentIncident provider result assertions

[<Fact>]
let ``invalid observation command raises error`` () =
  let _, provider = setupWithIncident()
  Assert.Throws<ValidationError>(fun () -> ignore (handler observationCommands provider [| "foo" |]))

[<Fact>]
let ``observation add requires incident`` () =
  let _, provider = setupWithoutIncident()
  Assert.Throws<ValidationError>(fun () -> ignore (handler observationCommands provider [| "add"; "Test" |]))

[<Fact>]
let ``observation add requires description`` () =
  let _, provider = setupWithIncident()
  let args = [| "add" |]
  let result = handler observationCommands provider args
  assertError result

[<Fact>]
let ``observation add sets description`` () =
  let start, provider = setupWithIncident()
  let description = "Test observation"
  let args = [| "add"; description |]
  let result = handler observationCommands provider args
  let assertions incident =
    let observation = List.find (fun e -> e.Type = EventType.Observation) incident.Events
    Assert.Equal(description, observation.Description)
  assertOnCurrentIncident provider result assertions

[<Fact>]
let ``observation add defaults time`` () =
  let start, provider = setupWithIncident()
  let args = [| "add"; "Observation" |]
  let result = handler observationCommands provider args
  assertOnCurrentIncident provider result (assertHasTime (fun e -> e.Type = EventType.Observation) start)

[<Fact>]
let ``observation add sets specified time`` () =
  let _, provider = setupWithIncident()
  let args = [| "add"; "Observation"; "2030-02-03T08:00" |]
  let result = handler observationCommands provider args
  let assertions incident =
    let observation = List.find (fun e -> e.Type = EventType.Observation) incident.Events
    Assert.True(observation.Time.IsPrecise)
    Assert.Equal(2030, observation.Time.Timestamp.Year)
    Assert.Equal(2, observation.Time.Timestamp.Month)
    Assert.Equal(3, observation.Time.Timestamp.Day)
  assertOnCurrentIncident provider result assertions

[<Fact>]
let ``observation list requires incidnet`` () =
  let _, provider = setupWithoutIncident()
  Assert.Throws<ValidationError>(fun () -> ignore(handler observationCommands provider [| "list" |]))

[<Fact>]
let ``observation list combines results`` () =
  let _, provider = setupWithIncident()
  observed (Events.now()) "First" (expect (provider.Current()))
  |> observed (Events.now()) "Second"
  |> provider.Put
  |> ignore
  let result = handler observationCommands provider [| "list" |]
  let hasResults msg =
    Assert.Contains("First", msg)
    Assert.Contains("Second", msg)
  assertSuccessMessage hasResults result