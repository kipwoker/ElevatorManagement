module Draw

open Common
open Browser.Types
open Domain

type Pixel = float

type Point =
    { X: float
      Y: float }

type Line =
    { Start: Point
      End: Point }

type CanvasSettings =
    { Width: Pixel
      Height: Pixel
      Padding: Point }

type DrawObjects = {
    Lines: Line list
}

type ElevatorPicture = {
    Pivot: Point
    Height: Pixel
    Width: Pixel
    LeafShift: Pixel
}

let private createRectangleLines (leftDownPoint: Point, rightUpPoint: Point) =
    let leftUpPoint = {
        X = leftDownPoint.X
        Y = rightUpPoint.Y
    }
    let rightDownPoint = {
        X = rightUpPoint.X
        Y = leftDownPoint.Y
    }
    [
        {Start = leftDownPoint; End = leftUpPoint}
        {Start = leftUpPoint; End = rightUpPoint}
        {Start = rightUpPoint; End = rightDownPoint}
        {Start = rightDownPoint; End = leftDownPoint}
    ]

let private elevatorToObjects (elevatorPicture: ElevatorPicture) =
    let leftDownPoint = {
                    X = elevatorPicture.Pivot.X - elevatorPicture.Width / 2.0
                    Y = elevatorPicture.Pivot.Y
                }
    let rightUpPoint = {
                    X = elevatorPicture.Pivot.X + elevatorPicture.Width / 2.0
                    Y = elevatorPicture.Pivot.Y + elevatorPicture.Height
                }
    let leftLeaf = {
            Start = {
                X = elevatorPicture.Pivot.X - elevatorPicture.LeafShift
                Y = elevatorPicture.Pivot.Y
            }
            End = {
                X = elevatorPicture.Pivot.X - elevatorPicture.LeafShift
                Y = elevatorPicture.Pivot.Y + elevatorPicture.Height
            }
        }

    let rightLeaf = {
            Start = {
                X = elevatorPicture.Pivot.X + elevatorPicture.LeafShift
                Y = elevatorPicture.Pivot.Y
            }
            End = {
                X = elevatorPicture.Pivot.X + elevatorPicture.LeafShift
                Y = elevatorPicture.Pivot.Y + elevatorPicture.Height
            }
        }

    {
        Lines = createRectangleLines (leftDownPoint, rightUpPoint) |> List.append [leftLeaf;rightLeaf]
    }

let private createElevatorPicture (settings: ElevatorSettings) (coefficient: float) (pivotPosition: Point) (leafShift: Meter) =
    {
        Pivot = pivotPosition
        Height = settings.Height * coefficient
        Width = settings.Width * coefficient
        LeafShift = leafShift * coefficient
    }

let private prepareObjects (canvasSettings: CanvasSettings) (buildingSettings: BuildingSettings) =
    let elevator = buildingSettings.Elevator
    let floor = buildingSettings.Floor

    let buildingWidthMeter = floor.Width |> log "buildingWidthMeter"
    let buildingHeightMeter = (float floor.Count) * floor.Height  |> log "buildingHeightMeter"

    let leftDownPoint = canvasSettings.Padding
    let buildingWidthPixel = (float canvasSettings.Width) - leftDownPoint.X * 2.0 |> log "buildingWidthPixel"
    let coefficient = buildingWidthPixel / buildingWidthMeter  |> log "coefficient"
    let buildingHeightPixel = buildingHeightMeter * coefficient |> log "buildingHeightPixel"

    let rightUpPoint =
        { X = leftDownPoint.X + buildingWidthPixel
          Y = leftDownPoint.Y + buildingHeightPixel }

    let border = createRectangleLines (leftDownPoint, rightUpPoint)

    let floors = List.init floor.Count (fun index ->
        let shift = floor.Height * (float index) * coefficient
        {
            Start = {
                X = leftDownPoint.X
                Y = leftDownPoint.Y + shift
            }
            End = {
                X = rightUpPoint.X
                Y = leftDownPoint.Y + shift
            }
        }
    )

    //note: for keeping center
    let elevatorTotalWidth = elevator.Width + elevator.Padding * 2.0
    let elevatorsTotalWidth = (float elevator.Count) * elevatorTotalWidth
    let elevatorsLeftPadding = (buildingWidthMeter - elevatorsTotalWidth) / 2.0

    let elevators = List.init elevator.Count (fun index ->
        let pivotX = elevatorsLeftPadding + elevatorTotalWidth * (float index) + elevatorTotalWidth / 2.0
        let pivot = {
            X = leftDownPoint.X + pivotX * coefficient
            Y = leftDownPoint.Y
        }
        let elevatorPicture = createElevatorPicture elevator coefficient pivot 0.0
        elevatorToObjects elevatorPicture
    )

    [
        {
            Lines = border
        }
        {
            Lines = floors
        }
    ] |> List.append elevators


let transform point height =
    { X = point.X
      Y = (float height) - point.Y }

let drawLine (line: Line) (height: Pixel) (context: CanvasRenderingContext2D) =
    log' (sprintf "Draw\n%A\n->\n%A" line.Start line.End)
    let start = transform line.Start height
    let end' = transform line.End height
    context.beginPath ()
    context.moveTo (start.X, start.Y)
    context.lineTo (end'.X, end'.Y)
    context.closePath ()
    context.stroke ()

let drawHouse (context: CanvasRenderingContext2D) (canvasSettings: CanvasSettings) (buildingSettings: BuildingSettings) =
    let drawObjectsList = prepareObjects canvasSettings buildingSettings
    drawObjectsList |> List.iter (fun drawObjects ->
        drawObjects.Lines
        |> List.iter (fun line -> drawLine line canvasSettings.Height context |> ignore)
    )
