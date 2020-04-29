module Draw

open System
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
      Padding: Point
      QuantToRenderingTime : TimeSpan
    }

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

let private createElevatorPicture (settings: ElevatorSettings) (coefficient: float) (pivotPosition: Point) (leafShift: Unit) =
    {
        Pivot = pivotPosition
        Height = settings.Height * coefficient
        Width = settings.Width * coefficient
        LeafShift = leafShift * coefficient
    }

let private prepareObjects (canvasSettings: CanvasSettings) (building: Building) =
    let buildingSettings = building.Settings
    let elevatorSettings = buildingSettings.Elevator
    let floorSettings = buildingSettings.Floor

    let buildingWidthMeter = floorSettings.Width
    let buildingHeightMeter = (float floorSettings.Count) * floorSettings.Height

    let leftDownPoint = canvasSettings.Padding
    let buildingWidthPixel = (float canvasSettings.Width) - leftDownPoint.X * 2.0
    let coefficient = buildingWidthPixel / buildingWidthMeter
    let buildingHeightPixel = buildingHeightMeter * coefficient

    let rightUpPoint =
        { X = leftDownPoint.X + buildingWidthPixel
          Y = leftDownPoint.Y + buildingHeightPixel }

    let border = createRectangleLines (leftDownPoint, rightUpPoint)

    let floorLines = List.init floorSettings.Count (fun index ->
        let shift = floorSettings.Height * (float index) * coefficient
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
    let elevatorTotalWidth = elevatorSettings.Width + elevatorSettings.Padding * 2.0
    let elevatorsTotalWidth = (float elevatorSettings.Count) * elevatorTotalWidth
    let elevatorsLeftPadding = (buildingWidthMeter - elevatorsTotalWidth) / 2.0

    let elevators = building.Elevators
    let elevatorDrawObjects = elevators |> List.map (fun elevator ->
        let pivotX = elevatorsLeftPadding + elevatorTotalWidth * (float (elevator.Number - 1)) + elevatorTotalWidth / 2.0
        let pivot = {
            X = leftDownPoint.X + pivotX * coefficient
            Y = leftDownPoint.Y + elevator.VerticalPosition * coefficient
        }
        let elevatorPicture = createElevatorPicture elevatorSettings coefficient pivot 0.0
        elevatorToObjects elevatorPicture
    )

    [
        {
            Lines = border
        }
        {
            Lines = floorLines
        }
    ] |> List.append elevatorDrawObjects


let transform point height =
    { X = point.X
      Y = (float height) - point.Y }

let drawLine (line: Line) (height: Pixel) (context: CanvasRenderingContext2D) =
    //log' (sprintf "Draw\n%A\n->\n%A" line.Start line.End)
    let start = transform line.Start height
    let end' = transform line.End height
    context.beginPath ()
    context.moveTo (start.X, start.Y)
    context.lineTo (end'.X, end'.Y)
    context.closePath ()
    context.stroke ()

let clearAll (context: CanvasRenderingContext2D) (canvasSettings: CanvasSettings) =
    context.clearRect(0.0, 0.0, canvasSettings.Width, canvasSettings.Height)

let drawBuilding (context: CanvasRenderingContext2D) (canvasSettings: CanvasSettings) (building: Building) =
    clearAll context canvasSettings

    let drawObjectsList = prepareObjects canvasSettings building
    drawObjectsList |> List.iter (fun drawObjects ->
        drawObjects.Lines
        |> List.iter (fun line -> drawLine line canvasSettings.Height context |> ignore)
    )
