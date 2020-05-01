module Render

open Browser.Dom
open Common
open Domain
open Draw
open Browser.Types
open Fable.Core.JS

type Settings = {
    CanvasSettings: CanvasSettings
    BuildingSettings: BuildingSettings
}

type Key =
    | Digit of int

type DomEvent = 
    | KeyDown of Key
    | KeyUp of Key

type EventList = {
    mutable queue: DomEvent list
}

let events = {
    queue = []
}

let keydown (event : KeyboardEvent)  =
    let result = 
        match int event.keyCode with
        | 49 -> events.queue <- events.queue @ [Digit 1 |> KeyDown]
        | 50 -> events.queue <- events.queue @ [Digit 2 |> KeyDown]
        | 51 -> events.queue <- events.queue @ [Digit 3 |> KeyDown]
        | _ -> ()
    result |> ignore

let keyup (event : KeyboardEvent)  =
    let result = 
        match int event.keyCode with
        | 49 -> events.queue <- events.queue @ [Digit 1 |> KeyUp]
        | 50 -> events.queue <- events.queue @ [Digit 2 |> KeyUp]
        | 51 -> events.queue <- events.queue @ [Digit 3 |> KeyUp]
        | _ -> ()
    result |> ignore

let createFloors (floorSettings : FloorSettings) : Floor list =
    List.init 
        floorSettings.Count 
        (fun index -> {
            Number = index + 1
        })

let createElevators (elevatorSettings : ElevatorSettings) (firstFloor : Floor) : Elevator list =
    List.init 
        elevatorSettings.Count 
        (fun index -> {
            Number = index + 1
            State = Idle firstFloor
            VerticalPosition = 0.0
        })    

let rec applyDomEvents (queue : DomEvent list) (building : Building) =
    match queue with
    | [] -> building
    | event::tail ->
        match event |> log "event" with
        | KeyDown key ->
            match key  |> log "key" with
            | Digit digit ->
                let maybeFloor = building.Floors |> List.tryFind (fun floor -> floor.Number = digit)
                match maybeFloor |> log "floor" with
                | Some floor ->
                    let callEvent = Call (building.Elevators.Head, floor)
                    let building' = applyDomainEvent callEvent building
                    applyDomEvents tail building'
                | None -> applyDomEvents tail building
        | KeyUp _ -> applyDomEvents tail building


let rec loop building context canvasSettings = 
    let queue = events.queue
    events.queue <- []

    let building' = building |> (applyDomEvents queue >> lifecycle)

    drawBuilding context canvasSettings building'
    
    setTimeout
        (fun () -> loop building' context canvasSettings |> ignore)
        (int canvasSettings.QuantToRenderingTime.TotalMilliseconds)
    |> ignore


let init (settings: Settings) =
    let canvas = document.getElementById("playground") :?> Browser.Types.HTMLCanvasElement
    let context = canvas.getContext_2d()
    let canvasSettings = settings.CanvasSettings

    canvas.width <- float canvasSettings.Width
    canvas.height <- float canvasSettings.Height
    
    document.onkeydown <- keydown
    document.onkeyup <- keyup

    
    let buildingSettings = settings.BuildingSettings

    let floors = createFloors buildingSettings.Floor
    let elevators = createElevators buildingSettings.Elevator floors.Head

    let building = {
        Settings = buildingSettings
        Elevators = elevators
        Floors = floors
    }

    loop building context canvasSettings

