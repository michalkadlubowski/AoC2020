module day6

open System
open System.Linq

let inBoth (a: string) (b: string) : string =
    let aChars = Seq.toList a
    let bChars = Seq.toList b
    System.String (aChars.Intersect bChars |> Seq.toArray)

let solution1 () = 
    let joined = readLines("data\\06-01.txt") |>
                    splitSeq (fun x-> x = "") |>
                        Seq.map(fun s -> Seq.reduce(fun x y -> x + y) s)
    Seq.map(fun s -> Seq.toArray s |> Seq.distinct |> Seq.length) joined |> Seq.sum

let solution2 () = 
    let joined = readLines("data\\06-01.txt") |>
                    splitSeq (fun x-> x = "") |>
                        Seq.map(fun s -> Seq.reduce(fun x y -> inBoth x  y) s)
    Seq.map(fun s -> Seq.toArray s |> Seq.distinct |> Seq.length) joined |> Seq.sum