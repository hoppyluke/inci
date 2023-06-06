module Inci.Core.Events

open System

let at time =
    EventTime(time, true)
    
let about time =
    EventTime(time, false)

// TODO: custom parsing logic
// 12:34? = about 12:34
// 12:34 = at 12:34
// assume local time
// 12:34u = UTC
// 12:34u? = about 12:34 UTC
// dates are always in the past - so if time > now then date = yesterday
// e.g. 23:59 = last night
// support date 2023-05-10

// Approx. regex:
// ((\d{4}-\d{2}-\d{2})[ tT])?\d{1,2}:\d{2}(:\d{2})?(\.\d+)?[uU]?\??
let parse t =
    at(DateTimeOffset.Parse(t))