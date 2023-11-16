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

let private expect value =
  match value with
  | Some x -> x
  | None -> failwith "Value expected"

let private setupWithoutIncident () =
  InMemoryCollection.Create().ToProvider()

let private setupWithIncident () =
  let collection = InMemoryCollection.Create()
  declare "Test incident" (Events.nowish())
  |> collection.Put
  |> collection.Select
  |> ignore
  collection.ToProvider()

[<Fact>]
let ``declare errors with no args`` () =
  let provider = setupWithoutIncident()
  let result = declareCommand provider Array.empty
  CommandAssertion.isError result

[<Fact>]
let ``declare succeeds without time`` () =
  let provider = setupWithoutIncident()
  let result = declareCommand provider [| "test name" |]
  CommandAssertion.hasIncident provider
  CommandAssertion.successMessage "test name" result
  CommandAssertion.lastEventWasNowish provider

[<Fact>]
let ``declare errors with invalid time`` () =
  let provider = setupWithIncident()
  CommandAssertion.failsValidation declareCommand [| "test name"; "foo" |] provider

[<Fact>]
let ``declare succeeds with valid time`` () =
  let provider = setupWithoutIncident()
  let result = declareCommand provider [| "test name"; "12:34" |]
  CommandAssertion.isSuccess result

[<Fact>]
let ``declare sets current incident`` () =
  let provider = setupWithoutIncident()
  let _result = declareCommand provider [| "test name" |]
  CommandAssertion.hasIncident provider

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``which without incident respects error option`` (shouldFail : bool) =
  let provider = setupWithoutIncident()
  let result = whichCommand provider shouldFail
  match shouldFail with
  | true -> CommandAssertion.errorMessage "No current incident" result
  | false -> CommandAssertion.successMessage "No current incident" result

[<Fact>]
let ``which returns current incident details`` () =
  let provider = setupWithIncident()
  let result = whichCommand provider true
  CommandAssertion.isSuccess result

[<Fact>]
let ``resolve without incident raises error`` () =
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation resolveCommand [| "12:34" |] provider 

[<Fact>]
let ``resolve succeeds with incident`` () =
  let provider = setupWithIncident()
  let result = resolveCommand provider [| |]
  CommandAssertion.isSuccess result

[<Fact>]
let ``resolve defaults time`` () =
  let provider = setupWithIncident()
  let _result = resolveCommand provider [| |]
  CommandAssertion.lastEventWasNowish provider

[<Fact>]
let ``resolve sets time`` () =
  let provider = setupWithIncident()
  let result = resolveCommand provider [| "2030-02-03T08:00" |]
  CommandAssertion.successMessage "2030-02-03" result

[<Fact>]
let ``invalid observation command raises error`` () =
  let provider = setupWithIncident()
  CommandAssertion.failsValidation (handler observationCommands) [| "foo" |] provider

[<Fact>]
let ``observation add requires incident`` () =
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation (handler observationCommands) [| "add"; "Test" |] provider

[<Fact>]
let ``observation add requires description`` () =
  let provider = setupWithIncident()
  let result = handler observationCommands provider [| "add" |]
  CommandAssertion.isError result

[<Fact>]
let ``observation add sets description`` () =
  let provider = setupWithIncident()
  let description = "Test observation"
  let result = handler observationCommands provider [| "add"; description |]
  CommandAssertion.successMessage description result

[<Fact>]
let ``observation add defaults time`` () =
  let provider = setupWithIncident()
  let _result = handler observationCommands provider [| "add"; "Observation" |]
  CommandAssertion.lastEventWasNowish provider

[<Fact>]
let ``observation add sets specified time`` () =
  let provider = setupWithIncident()
  let result = handler observationCommands provider [| "add"; "Observation"; "2030-02-03T08:00" |]
  CommandAssertion.successMessage "2030-02-03" result

[<Fact>]
let ``observation list requires incidnet`` () =
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation (handler observationCommands) [| "list" |] provider

[<Fact>]
let ``observation list combines results`` () =
  let provider = setupWithIncident()
  observed (Events.now()) "First" (expect (provider.Current()))
  |> observed (Events.now()) "Second"
  |> provider.Put
  |> ignore
  let result = handler observationCommands provider [| "list" |]
  CommandAssertion.successMessage "First" result
  CommandAssertion.successMessage "Second" result

[<Fact>]
let ``action add adds new action`` () =
  let provider = setupWithIncident()
  let result = handler actionCommands provider [| "add"; "Testing" |]
  CommandAssertion.successMessage "Testing" result

[<Fact>]
let ``action add requires description`` () =
  let provider = setupWithIncident()
  let result = handler actionCommands provider [| "add" |]
  CommandAssertion.errorMessage "usage" result

[<Fact>]
let ``action add defaults time`` () =
  let provider = setupWithIncident()
  let _result = handler actionCommands provider [| "add"; "Time testing" |]
  CommandAssertion.lastEventWasNowish provider

[<Fact>]
let ``action add uses sets specified time`` () =
  let provider = setupWithIncident()
  let result = handler actionCommands provider [| "add"; "Time testing"; "2000-08-02" |]
  CommandAssertion.successMessage "2000-08-02" result

[<Fact>]
let ``action list requires incident`` () =
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation (handler actionCommands) [| "list" |] provider

[<Fact>]
let ``action list action builds list`` () =
  let provider = setupWithIncident()
  handler actionCommands provider [| "add"; "First action" |] |> ignore
  handler actionCommands provider [| "add"; "Second action" |] |> ignore
  let result = handler actionCommands provider [| "list" |]
  CommandAssertion.successMessage "First action" result
  CommandAssertion.successMessage "Second action" result

