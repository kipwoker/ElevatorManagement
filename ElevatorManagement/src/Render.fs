module Render

open Browser.Dom
open Domain
open Draw

type Settings = {
    CanvasSettings: CanvasSettings
    BuildingSettings: BuildingSettings
}

let init (settings: Settings) =
    let canvasSettings = settings.CanvasSettings
    let buildingSettings = settings.BuildingSettings

    let canvas = document.getElementById("playground") :?> Browser.Types.HTMLCanvasElement

    canvas.width <- float canvasSettings.Width
    canvas.height <- float canvasSettings.Height

    let context = canvas.getContext_2d()

    drawHouse context canvasSettings buildingSettings

