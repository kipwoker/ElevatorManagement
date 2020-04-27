module Domain

type Meter = float

type Count = int

type MeterPerSec = float

type FloorSettings =
    { Count: Count
      Height: Meter
      Width: Meter }

type ElevatorSettings =
    { Count: Count
      Height: Meter
      Width: Meter
      Speed: MeterPerSec
      Padding: Meter }

type BuildingSettings =
    { Floor: FloorSettings
      Elevator: ElevatorSettings }

type Building =
    { Settings: BuildingSettings }

//type DomainState = {
//
//}
