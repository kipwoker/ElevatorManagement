module Domain

open Common
open Fable.Core.JS

type Unit = float
type Quant = float

type Count = int

type UnitPerQuant = float

type FailType = 
  | ElevatorMoves

type FloorSettings =
    { Count: Count
      Height: Unit
      Width: Unit }

type ElevatorSettings =
    { Count: Count
      Height: Unit
      Width: Unit
      Speed: UnitPerQuant
      Padding: Unit }

type BuildingSettings =
    { 
      Floor: FloorSettings
      Elevator: ElevatorSettings 
      QuantPerMove : Quant
    }

type Floor = {
  Number: int
}

type Destination = Floor

type ElevatorState =
  | Idle of Floor
  | Ascent of Destination
  | Descent of Destination

type Elevator = {
  Number: int
  State: ElevatorState
  VerticalPosition: Unit
}


type Building =
    { 
      Settings: BuildingSettings
      Elevators: Elevator list
      Floors: Floor list
    }

let tryCallElevator (destinationFloor: Floor) (elevator: Elevator) =
  match elevator.State with
  | Idle floor' -> 
    {
      Number = elevator.Number
      State = 
        if destinationFloor.Number > floor'.Number 
        then 
          Ascent destinationFloor
        else if destinationFloor.Number < floor'.Number 
        then
          Descent destinationFloor
        else
          Idle floor'
      VerticalPosition = elevator.VerticalPosition
    } |> Ok
  | _ -> ElevatorMoves |> Fail

let lifecycle (building: Building) =
  let quantCountPerMove = building.Settings.QuantPerMove
  let elevatorSettings = building.Settings.Elevator
  let floorSettings = building.Settings.Floor
  let speed = elevatorSettings.Speed
  let elevators = building.Elevators
                  |> List.map (
                        fun elevator -> 
                          match elevator.State with
                          | Idle _ -> elevator
                          | Ascent destination -> 
                            let destinationHeight = (float (destination.Number - 1)) * floorSettings.Height |> log "destinationHeight"
                            let newVerticalPosition = Math.min(destinationHeight, elevator.VerticalPosition + speed * quantCountPerMove) |> log "newVerticalPosition"
                            {
                              Number = elevator.Number
                              State = if destinationHeight = newVerticalPosition then Idle destination else elevator.State
                              VerticalPosition = newVerticalPosition
                            }
                          | Descent destination -> 
                            let destinationHeight = (float (destination.Number - 1)) * floorSettings.Height |> log "destinationHeight"
                            let newVerticalPosition = Math.max(destinationHeight, elevator.VerticalPosition - speed * quantCountPerMove) |> log "newVerticalPosition"
                            {
                              Number = elevator.Number
                              State = if destinationHeight = newVerticalPosition then Idle destination else elevator.State
                              VerticalPosition = newVerticalPosition
                            }
                     )
  {
    Settings = building.Settings
    Elevators = elevators
    Floors = building.Floors
  }                 