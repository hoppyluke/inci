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

let private selectCommandGroup name =
  match name with
  | "observation" -> observationCommands
  | "action" -> actionCommands
  | "alert" -> alertCommands
  | _ -> failwith $"Invalid command group: {name}"

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

[<Theory>]
[<InlineData("observation")>]
[<InlineData("action")>]
[<InlineData("alert")>]
let ``invalid command raises error`` group =
  let provider = setupWithIncident()
  let commandGroup = selectCommandGroup group
  CommandAssertion.failsValidation (handler commandGroup) [| "foo" |] provider

[<Theory>]
[<InlineData("observation")>]
[<InlineData("action")>]
[<InlineData("alert")>]
let ``event list requires incident`` group =
  let commandGroup = selectCommandGroup group
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation (handler commandGroup) [| "list" |] provider

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``event list returns list`` group addVerb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithIncident()
  ignore (handler commandGroup provider [| addVerb; "Event 1" |])
  ignore (handler commandGroup provider [| addVerb; "Event 2" |])
  let result = handler commandGroup provider [| "list" |]
  CommandAssertion.successMessage "Event 1" result
  CommandAssertion.successMessage "Event 2" result

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``add event requires incident`` group verb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithoutIncident()
  CommandAssertion.failsValidation (handler commandGroup) [| verb; "Test" |] provider

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``add event requires description`` group verb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithIncident()
  let result = handler commandGroup provider [| verb |]
  CommandAssertion.isError result

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``add event sets description`` group verb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithIncident()
  let description = "Test description"
  let result = handler commandGroup provider [| verb; description |]
  CommandAssertion.successMessage description result

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``add event defaults time if missing`` group verb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithIncident()
  let _result = handler commandGroup provider [| verb; "Something" |]
  CommandAssertion.lastEventWasNowish provider

[<Theory>]
[<InlineData("observation", "add")>]
[<InlineData("action", "add")>]
[<InlineData("alert", "fired")>]
let ``add event uses provided time`` group verb =
  let commandGroup = selectCommandGroup group
  let provider = setupWithIncident()
  let result = handler commandGroup provider [| verb; "Something"; "2030-02-03T08:00" |]
  CommandAssertion.successMessage "2030-02-03" result
