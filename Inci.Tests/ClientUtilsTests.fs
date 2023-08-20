module Inci.Client.UtilsTests

open Xunit
open Inci.Client.Utils

[<Theory>]
[<InlineData("Observation", "observation")>]
[<InlineData("obs", "observation")>]
[<InlineData(" alert ", "alert")>]
[<InlineData("alr", "alert")>]
[<InlineData("mONiTOR", "monitor")>]
[<InlineData("mon", "monitor")>]
[<InlineData("ACTION", "action")>]
[<InlineData("act", "action")>]
[<InlineData("declare ", "declare")>]
[<InlineData("dec", "declare")>]
[<InlineData("  resolve", "resolve")>]
[<InlineData("res", "resolve")>]
[<InlineData("rename", "rename")>]
let ```canonicalName selects correct representation`` name expectedName =
    let canonical = canonicalName name
    Assert.Equal(expectedName, canonical)