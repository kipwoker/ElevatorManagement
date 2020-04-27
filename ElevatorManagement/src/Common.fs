module Common

open Fable.Core.JS

let log prefix msg =
    console.log (prefix + " = " + msg.ToString())
    msg

let log' msg =
    console.log(msg) |> ignore