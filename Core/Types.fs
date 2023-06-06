namespace Inci.Core

open System

type EventTime =
    struct
        val Timestamp: DateTimeOffset
        val IsPrecise: bool

        new(timestamp, isPrecise) = { Timestamp = timestamp; IsPrecise = isPrecise }
    end

type Observation =
    { Id: string
      Time: EventTime
      Description: string }

type Action =
    { Id: string
      Time: EventTime
      Description: string }

type Alert =
    { Id: string
      Time: EventTime
      Description: string
      IsFired: bool }

type Monitor =
    { Id: string
      Time: EventTime
      Description: string
      IsDown: bool }

type Declaration =
    { Time: EventTime
      IsResolved: bool }

type Event =
    | Observation
    | Action
    | Monitor
    | Alert
    | Resolution

type Incident =
    { Id: Guid
      Name: string
      Events: Event list }