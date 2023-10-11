module Inci.Core.EventsTests

open System
open Xunit
open Inci.Core.Events


let pastTimeToday () =
    let now = DateTimeOffset.Now
    if now.Hour = 0 && now.Minute = 0 then
        System.Threading.Thread.Sleep(TimeSpan.FromMinutes(1))
    DateTimeOffset.Now.AddMinutes(-1)

let futureTimeToday () =
    let now = DateTimeOffset.Now
    if now.Hour = 23 && now.Minute = 59 then
        System.Threading.Thread.Sleep(TimeSpan.FromMinutes(1))
    DateTimeOffset.Now.AddMinutes(1)

[<Fact>]
let ``parse handles precise time to the minute in the past`` () =
    let targetTime = pastTimeToday ()
    let s = targetTime.ToString("HH:mm")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(0, result.Timestamp.Second)
    Assert.Equal(0, result.Timestamp.Millisecond)

[<Fact>]
let ``parse handles precise time to the second in the past`` () =
    let targetTime = pastTimeToday ()
    let s = targetTime.ToString("HH:mm:ss")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)
    Assert.Equal(0, result.Timestamp.Millisecond)

[<Fact>]
let ``parse handles precise time to the ms in the past`` () =
    let targetTime = pastTimeToday ()
    let s = targetTime.ToString("HH:mm:ss.fff")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)
    Assert.Equal(targetTime.Millisecond, result.Timestamp.Millisecond)

[<Fact>]
let ``parse sets future precise times as previous day`` () =
    let targetTime = futureTimeToday ()
    let expectedDay = targetTime.Date.AddDays(-1)
    let s = targetTime.ToString("HH:mm")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(expectedDay.Year, result.Timestamp.Year)
    Assert.Equal(expectedDay.Month, result.Timestamp.Month)
    Assert.Equal(expectedDay.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)


[<Fact>]
let ``parse handles precise date times in the past`` () =
    let targetTime = DateTimeOffset.Now.AddDays(-1)
    let s = targetTime.ToString("yyyy-MM-ddTHH:mm:ss")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)

[<Fact>]
let ``parse handles precise date times in the future`` () =
    let targetTime = DateTimeOffset.Now.AddDays(1)
    let s = targetTime.ToString("yyyy-MM-ddTHH:mm:ss")
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.True(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)

[<Fact>]
let ``parse handles approx. time to the second in the past`` () =
    let targetTime = pastTimeToday ()
    let s = targetTime.ToString("HH:mm:ss") + "?"
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)
    Assert.Equal(0, result.Timestamp.Millisecond)

[<Fact>]
let ``parse handles approx. time to the minute in the future`` () =
    let targetTime = futureTimeToday ()
    let expectedDay = targetTime.Date.AddDays(-1)
    let s = targetTime.ToString("HH:mm") + "?"
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(expectedDay.Year, result.Timestamp.Year)
    Assert.Equal(expectedDay.Month, result.Timestamp.Month)
    Assert.Equal(expectedDay.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(0, result.Timestamp.Second)
    Assert.Equal(0, result.Timestamp.Millisecond)

[<Fact>]
let ``parse treats null as now ish`` () =
    let parseResult = parse null
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(DateTimeOffset.UtcNow, result.Timestamp, TimeSpan.FromMilliseconds(100))

[<Fact>]
let ``parse treats empty string as now ish`` () =
    let parseResult = parse ""
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(DateTimeOffset.UtcNow, result.Timestamp, TimeSpan.FromMilliseconds(100))

[<Fact>]
let ``parse handles approx. date and time in the past`` () =
    let targetTime = DateTimeOffset.Now.AddDays(-1)
    let s = targetTime.ToString("yyyy-MM-ddTHH:mm:ss") + "?"
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)

[<Fact>]
let ``parse handles approx. date and time in the future`` () =
    let targetTime = DateTimeOffset.Now.AddDays(1)
    let s = targetTime.ToString("yyyy-MM-ddTHH:mm:ss") + "?"
    let parseResult = parse s
    Assert.True(Option.isSome parseResult)
    let result = Option.get parseResult
    Assert.False(result.IsPrecise)
    Assert.Equal(targetTime.Year, result.Timestamp.Year)
    Assert.Equal(targetTime.Month, result.Timestamp.Month)
    Assert.Equal(targetTime.Day, result.Timestamp.Day)
    Assert.Equal(targetTime.Hour, result.Timestamp.Hour)
    Assert.Equal(targetTime.Minute, result.Timestamp.Minute)
    Assert.Equal(targetTime.Second, result.Timestamp.Second)

[<Fact>]
let ``formatTime handles exact time`` () =
    let target = DateTimeOffset.Now
    let result = formatTime (at target)
    Assert.Equal(target.ToString("yyyy-MM-dd HH:mm:ssK"), result)

[<Fact>]
let ``formatTime handles date only`` () =
    let target = DateTimeOffset.Parse("2023-01-01")
    let result = formatTime (at target)
    Assert.Equal("2023-01-01", result)

[<Fact>]
let ``formatTime handles approx time`` () =
    let target = DateTimeOffset.Now
    let result = formatTime (about target)
    Assert.Equal(target.ToString("yyyy-MM-dd HH:mm:ssK?"), result)

[<Fact>]
let ``formatTime handles approx date only`` () =
    let target = DateTimeOffset.Parse("2023-01-01")
    let result = formatTime (about target)
    Assert.Equal("2023-01-01?", result)