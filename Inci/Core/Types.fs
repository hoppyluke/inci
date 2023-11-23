namespace Inci.Core

open System

module Internal =
    let thenBy x y comparison =
        match comparison with
        | 0 -> compare x y
        | n -> n

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

[<CustomComparison; CustomEquality>]
type Event =
    { Id: string
      Type: EventType
      Time: EventTime
      Description: string
      IsResolved: bool }
    
    interface IComparable<Event> with
        member this.CompareTo other =
            compare this.Time other.Time
            |> Internal.thenBy this.Id other.Id
            |> Internal.thenBy this.IsResolved other.IsResolved
    
    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Event as other -> (this :> IComparable<Event>).CompareTo other
            | _ -> invalidArg (nameof obj) "Not an Event"

    override this.Equals obj =
        match obj with
        | :? Event as other -> this.Id = other.Id && this.IsResolved = other.IsResolved
        | _ -> false

    override this.GetHashCode () =
        this.Id.GetHashCode() ||| this.IsResolved.GetHashCode()

type Incident =
    { Id: Guid
      Name: string
      Events: Event list }