module Inci.Core.EventTimeTests

open Inci.Core
open System
open Xunit

[<Fact>]
let ``compare sorts by date (precise)`` () =
    let e1 = EventTime(DateTimeOffset.UtcNow, true)
    let e2 = EventTime(DateTimeOffset.UtcNow.AddMinutes(10), true)
    Assert.True(e1 < e2)

[<Fact>]
let ``compare sorts by date (approx)`` () =
    let e1 = EventTime(DateTimeOffset.UtcNow, false)
    let e2 = EventTime(DateTimeOffset.UtcNow.AddMinutes(10), false)
    Assert.True(e1 < e2)

[<Fact>]
let ``compare sorts by approx. first`` () =
    let t = DateTimeOffset.UtcNow
    let e1 = EventTime(t, true)
    let e2 = EventTime(t, false)
    Assert.True(e1 > e2)

[<Fact>]
let ``comparing events sorts by time`` () =
    let t1 = EventTime(DateTimeOffset.UtcNow, false)
    let t2 = EventTime(DateTimeOffset.UtcNow.AddMinutes(10), false)
    let e1 = { Id="2"; Type=Observation; Time=t1; Description="Test"; IsResolved=false}
    let e2 = { Id="1"; Type=Observation; Time=t2; Description="Test"; IsResolved=false}
    Assert.True(e1 < e2)

[<Fact>]
let ``comparing events sorts by time then ID`` () =
    let t = EventTime(DateTimeOffset.UtcNow, false)
    let e1 = { Id="a"; Type=Observation; Time=t; Description="Test"; IsResolved=false}
    let e2 = { Id="b"; Type=Observation; Time=t; Description="Test"; IsResolved=false}
    Assert.True(e1 < e2)

[<Fact>]
let ``comparing events sorts by time then ID then IsResolved`` () =
    let t = EventTime(DateTimeOffset.UtcNow, false)
    let e1 = { Id="a1"; Type=Alert; Time=t; Description="Test"; IsResolved=false}
    let e2 = { Id="a1"; Type=Alert; Time=t; Description="Test"; IsResolved=true}
    Assert.True(e1 < e2)