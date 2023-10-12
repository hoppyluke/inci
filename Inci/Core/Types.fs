namespace Inci.Core

open System

type EventTime =
    struct
        val Timestamp: DateTimeOffset
        val IsPrecise: bool

        [<System.Text.Json.Serialization.JsonConstructor>]
        new(timestamp, isPrecise) = { Timestamp = timestamp; IsPrecise = isPrecise }
    end

type EventType =
    | Observation
    | Action
    | Monitor
    | Alert
    | Declaration

type Event =
    { Id: string
      Type: EventType
      Time: EventTime
      Description: string
      IsResolved: bool }

type Incident =
    { Id: Guid
      Name: string
      Events: Event list }