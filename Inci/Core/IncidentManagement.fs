module Inci.Core.IncidentManagement

open System

let private addEvent e i =
    { i with Events = e :: i.Events }

let declare name start =
    { Id = Guid.NewGuid()
      Name = name 
      Events = [Declaration(start, false)] }

let resolve time i =
    addEvent (Declaration(time, true)) i

let rename name i =
    { i with Name = name }

let observed time s i =
    let id = "o1"
    addEvent (Observation(id, time, s)) i