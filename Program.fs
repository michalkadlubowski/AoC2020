// Learn more about F# at http://fsharp.org
module AoC2020

open System
open day17

[<EntryPoint>]
let main argv =
    solution1 () |> printfn "%i" 
    solution2 () |> printfn "%i" 
    0 // return an integer exit code
