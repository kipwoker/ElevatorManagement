module App

open Domain
open Draw
open Render
open Browser.Dom

window.onload <- fun _ ->
    init({
        CanvasSettings = {
            Width = 900.0
            Height = 900.0
            Padding = {
                X = 30.0
                Y = 30.0
            }
        };
        BuildingSettings = {
            Floor = {
                Height = 110.0
                Width = 800.0
                Count = 7
            };
            Elevator = {
                Count = 3
                Height = 70.0
                Width = 40.0
                Speed = 30.0
                Padding = 15.0
            }
        }
    })
    |> ignore