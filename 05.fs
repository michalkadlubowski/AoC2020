module day5

open System

let toBinaryString (input:string) : string = 
    input.Replace("B","1").Replace("F","0").Replace("R","1").Replace("L","0")

let toNumber (input:string) : int =
    let bin = toBinaryString input 
    Convert.ToInt32(bin,2)

let mapInput (input:seq<string>) =
    input |> Seq.map toNumber

let findMissing (input:seq<string>) =
    snd (input |> Seq.map toNumber |>
        Seq.sort |> Seq.pairwise |>
            Seq.find(fun el -> (fst el) + 1 <> snd el)) - 1

let solution1 () = 
    readLines("data\\05-01.txt") |>
         mapInput |> Seq.max 

let solution2 () = 
    readLines("data\\05-01.txt") |>
        findMissing