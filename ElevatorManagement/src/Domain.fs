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

type Direction =
  | Up
  | Down

type ElevatorState =
  | Idle of Floor
  | Move of Direction * Destination

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

type DomainEvent = 
  | Call of Elevator * Destination  

let tryCallElevator (destinationFloor: Floor) (elevator: Elevator) =
  match elevator.State with
  | Idle floor' -> 
    {
      Number = elevator.Number
      State = 
        if destinationFloor.Number > floor'.Number 
        then 
          Move (Up, destinationFloor)
        else if destinationFloor.Number < floor'.Number 
        then
          Move (Down, destinationFloor)
        else
          Idle floor'
      VerticalPosition = elevator.VerticalPosition
    } |> Ok
  | _ -> ElevatorMoves |> Fail

let rec private replaceElevator (elevator:Elevator) (elevators:Elevator list) =
  match elevators with
  | [] -> []
  | head::tail when head.Number = elevator.Number -> elevator::tail
  | head::tail -> head::(replaceElevator elevator tail)
    

let applyDomainEvent (event: DomainEvent) (building: Building) =
  match event with
  | Call (elevator, floor) -> 
    let callResult = tryCallElevator floor elevator
    match callResult with
    | Ok elevator' -> 
        {
            Settings = building.Settings
            Elevators = replaceElevator elevator' building.Elevators
            Floors = building.Floors
        }
    | Fail _ -> building

let private move speed quantCountPerMove height elevator direction (destination : Destination) =
  let destinationHeight = (float (destination.Number - 1)) * height
  let interval = speed * quantCountPerMove
  let newVerticalPosition =
    match direction with
    | Up -> Math.min(destinationHeight, elevator.VerticalPosition + interval)
    | Down -> Math.max(destinationHeight, elevator.VerticalPosition - interval)
  let newState =
    if destinationHeight = newVerticalPosition
    then Idle destination
    else elevator.State
  {
    Number = elevator.Number
    State = newState
    VerticalPosition = newVerticalPosition
  }

let lifecycle (building: Building) =
  let quantCountPerMove = building.Settings.QuantPerMove
  let elevatorSettings = building.Settings.Elevator
  let floorSettings = building.Settings.Floor
  let height = floorSettings.Height
  let speed = elevatorSettings.Speed
  let move' = move speed quantCountPerMove height
  let elevators = building.Elevators
                  |> List.map (
                        fun elevator -> 
                          match elevator.State with
                          | Idle _ -> elevator
                          | Move (direction, destination) -> move' elevator direction destination
                     )
  {
    Settings = building.Settings
    Elevators = elevators
    Floors = building.Floors
  }                 