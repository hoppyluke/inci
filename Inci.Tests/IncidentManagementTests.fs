module Inci.Core.IncidentManagementTests

open Xunit

open Inci.Core.IncidentManagement

[<Fact>]
let ``declare creates active incident`` () =
    let i = declare "Stuff broke" (Events.now())
    Assert.Contains(i.Events, fun e -> e.Type = Declaration && not e.IsResolved)

[<Fact>]
let ``resolve creates resolution event`` () =
    let resolutionTime = Events.now()
    let i = resolve resolutionTime (declare "Stuff broke" (Events.now()))
    Assert.Contains(i.Events, fun e -> e.Type = Declaration && e.IsResolved)

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
        | e when e.Type = Observation && e.Description = o -> true
        | _ -> false
    Assert.Contains(i2.Events, isObservation)
    

[<Fact>]
let ``observed sets id`` () =
    let i = declare "Stuff broke" (Events.now())
    let i2 = observed (Events.now()) "It's not good" i
    let o = List.find (fun e -> e.Type = Observation) i2.Events
    Assert.NotNull(o.Id)

[<Fact>]
let ``add event sets sequential ID`` () =
    let i = declare "Stuff broke" (Events.now())
            |> observed (Events.now()) "Obs 1"
            |> observed (Events.now()) "Obs 2"
            |> observed (Events.now()) "Obs 3"
    let e = List.head i.Events
    Assert.Equal(4, List.length(i.Events))
    Assert.Equal("o2", e.Id)
    Assert.Equal("Obs 3", e.Description)
    