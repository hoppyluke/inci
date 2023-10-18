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