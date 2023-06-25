module Inci.Core.IncidentManagementTests

open Xunit

open Inci.Core.IncidentManagement

[<Fact>]
let ``declare creates active incident`` () =
    let i = declare "Stuff broke" (Events.now())
    let isDeclaration e =
        match e with
        | Declaration(_, isResolved) -> not isResolved
        | _ -> false
    Assert.Contains(i.Events, isDeclaration)

[<Fact>]
let ``resolve creates active incident`` () =
    let resolutionTime = Events.now()
    let i = resolve resolutionTime (declare "Stuff broke" (Events.now()))
    let isResolution e =
        match e with
        | Declaration(_, isResolved) -> isResolved
        | _ -> false
    Assert.Contains(i.Events, isResolution)

[<Fact>]
let ``rename changes name only`` () =
    let i = declare "Stuff broke" (Events.nowish())
    let newName = "Everything broke"
    let i2 = rename newName i
    Assert.Equal(newName, i2.Name)
    Assert.Equal(i.Id, i2.Id)

[<Fact>]
let ``observed adds observation`` () =
    let i = declare "Stuff broke" (Events.now())
    let o = "It's not good"
    let i2 = observed (Events.now()) o i
    let isObservation e =
        match e with 
        | Observation(_, _, obs) when obs = o -> true
        | _ -> false
    Assert.Contains(i2.Events, isObservation)
    

[<Fact>]
let ``observed sets id`` () =
    let i = declare "Stuff broke" (Events.now())
    let i2 = observed (Events.now()) "It's not good" i
    let o =
        i2.Events
        |> List.find (function | Observation(_) -> true | _ -> false)
    let hasId = match o with
                | Observation(id, time, desc) -> not (System.String.IsNullOrWhiteSpace(id))
                | _ -> false
    Assert.True(hasId)