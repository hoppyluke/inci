namespace Inci.Core

open System

type EventTime =
    struct
        val Timestamp: DateTimeOffset
        val IsPrecise: bool

        new(timestamp, isPrecise) = { Timestamp = timestamp; IsPrecise = isPrecise }
    end

type Event =
    | Observation of id: string * time : EventTime * description : string
    | Action of id: string * time : EventTime * description : string
    | Monitor of id: string * time : EventTime * description : string * isDown : bool
    | Alert of id: string * time : EventTime * description : string * isFired: bool
    | Declaration of time : EventTime * isResolved : bool

type Incident =
    { Id: Guid
      Name: string
      Events: Event list }