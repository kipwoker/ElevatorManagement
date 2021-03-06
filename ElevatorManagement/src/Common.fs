module Common

open Fable.Core.JS

type Result<'T, 'E> = 
    | Ok of 'T
    | Fail of 'E

let log prefix msg =
    console.log (prefix + " = " + msg.ToString())
    msg

let log' msg =
    console.log(msg) |> ignore