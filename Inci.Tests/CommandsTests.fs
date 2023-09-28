module Inci.Client.CommandsTests

open System
open Xunit
open Inci.Core
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
    
      member this.ToProvider() =
        { Current = this.Current; Put = this.Put}

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
  let result = declareCommand (InMemoryCollection.Create().ToProvider()) args
  assertError result

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
