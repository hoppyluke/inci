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
    Assert.Equal(4, List.length(i.Events))
    let o0 = List.find (fun e -> e.Description = "Obs 1") i.Events
    Assert.Equal("o0", o0.Id)
    let o1 = List.find (fun e -> e.Description = "Obs 2") i.Events
    Assert.Equal("o1", o1.Id)
    let o2 = List.find (fun e -> e.Description = "Obs 3") i.Events
    Assert.Equal("o2", o2.Id)
    
[<Fact>]
let ``acted adds action`` () =
    let t = Events.now()
    let a = "Turned it off and on again"
    let i = declare "Stuff broke" (Events.now())
            |> acted t a
    let action = List.find (fun e -> e.Type = Action) i.Events
    Assert.Equal(t, action.Time)
    Assert.Equal(a, action.Description)
    Assert.False(action.IsResolved)

[<Fact>]
let ``alertFired adds alert`` () =
    let t = Events.now()
    let a = "CPU alert"
    let i = declare "Stuff broke" (Events.now())
            |> alertFired t a
    let alert = List.find (fun e -> e.Type = Alert) i.Events
    Assert.Equal(t, alert.Time)
    Assert.Equal(a, alert.Description)
    Assert.False(alert.IsResolved)

[<Fact>]
let ``down adds monitor down`` () =
    let t = Events.now()
    let m = "Availability"
    let i = declare "Stuff broke" (Events.now())
            |> down t m
    let monitor = List.find (fun e -> e.Type = Monitor) i.Events
    Assert.Equal(t, monitor.Time)
    Assert.Equal(m, monitor.Description)
    Assert.False(monitor.IsResolved)

[<Fact>]
let ``alertResolved adds resolution event`` () =
    let a = "Test"
    let i  = declare "Stuff broke" (Events.now()) |> alertFired (Events.now()) a
    let id = List.find (fun e -> e.Type = Alert) i.Events
             |> fun e -> e.Id
    let t = Events.now()
    let result = alertResolved t id i
    Assert.True(result.IsSome)
    let i2 = result.Value
    let resolved = List.find (fun e -> e.Type = Alert && e.IsResolved) i2.Events
    Assert.Equal(t, resolved.Time)
    Assert.Equal(a, resolved.Description)

[<Fact>]
let ``alertResolved handles missing ID`` () =
    let i  = declare "Stuff broke" (Events.now())
    let t = Events.now()
    let result = alertResolved t "a99" i
    Assert.True(result.IsNone)

[<Fact>]
let ``alertResolved ignores duplicate resolution`` () =
    let i  = declare "Stuff broke" (Events.now()) |> alertFired (Events.now()) "Test"
    let id = List.find (fun e -> e.Type = Alert) i.Events
             |> fun e -> e.Id
    let t1 = Events.now()
    let i2 = alertResolved t1 id i
    let t2 = Events.now()
    let result = alertResolved t2 id i2.Value
    Assert.True(result.IsSome)
    let i3 = result.Value
    let resolutions = List.filter (fun e -> e.Type = Alert && e.IsResolved) i3.Events
    let resolution = Assert.Single(resolutions)
    Assert.Equal(t1, resolution.Time)

[<Fact>]
let ``up adds resolution event`` () =
    let m = "Test"
    let i  = declare "Stuff broke" (Events.now()) |> down (Events.now()) m
    let id = List.find (fun e -> e.Type = Monitor) i.Events
             |> fun e -> e.Id
    let t = Events.now()
    let result = up t id i
    Assert.True(result.IsSome)
    let i2 = result.Value
    let resolved = List.find (fun e -> e.Type = Monitor && e.IsResolved) i2.Events
    Assert.Equal(t, resolved.Time)
    Assert.Equal(m, resolved.Description)

[<Fact>]
let ``up handles invalid type id`` () =
    let i = declare "Stuff broke" (Events.now())
            |> alertFired (Events.now()) "Alert"
    let alert = List.find (fun e -> e.Type = Alert) i.Events
    let i2 = up (Events.now()) alert.Id i
    Assert.True(i2.IsNone)
