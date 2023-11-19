module Inci.Client.Commands.CommandAssertion

open System
open Xunit
open Inci.Core
open Inci.Client.Commands
open Inci.Client.IO

let hasIncident provider =
    match provider.Current() with
    | Some i -> ()
    | None -> Assert.Fail("Expected provider to have an incident")

let isSuccess result =
    match result with
    | Error m -> Assert.Fail($"Expected success (error: {m})")
    | _ -> ()

let isError result =
    match result with
    | Success m -> Assert.Fail($"Expected error (success: {m})")
    | _ -> ()

let successMessage message result =
    match result with
    | Success m -> Assert.Contains(message, m)
    | Error m -> Assert.Fail($"Expected success (error: {m})")

let errorMessage message result =
    match result with
    | Error m -> Assert.Contains(message, m)
    | Success m -> Assert.Fail($"Expected error (success: {m})")

let private assertOnCurrent assertion provider =
    match provider.Current() with
    | Some i -> assertion i
    | None -> Assert.Fail("Expected provider to have an incident")

let lastEventWasNowish provider =
    let assertion incident =
        match incident.Events with
        | head :: _ -> Assert.False(head.Time.IsPrecise, $"Expected imprecise time: {head.Id} @ {head.Time.Timestamp}")
                       let b = DateTimeOffset.UtcNow.AddMinutes(-1)
                       let e = DateTimeOffset.UtcNow.AddMinutes(1)
                       Assert.True(head.Time.Timestamp >= b && head.Time.Timestamp <= e , $"Expected recent time: {head.Time.Timestamp}")
        | _ -> Assert.Fail("Expected a nowish event")
    assertOnCurrent assertion provider

let failsValidation command args provider =
    Assert.Throws<ValidationError>(fun () -> ignore (command provider args))