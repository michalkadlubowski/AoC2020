module day18

open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked

type Chunk = Number of int64 | Mulply | Add | ParenOpen | ParenClose 

let getValue chunk = 
    match chunk with 
        | Chunk.Number n -> n
        | _ -> raise <| ArgumentException()

let calculateValue1 (chunks: Chunk list) =
    let firstval = getValue chunks.[0]
    let res = chunks |> List.indexed |> List.fold(fun acc (i,c) ->
                                                    match c with
                                                        | Chunk.Mulply ->  acc * (getValue chunks.[i+1])
                                                        | Chunk.Add -> acc + (getValue chunks.[i+1])
                                                        | _ -> acc) firstval
    res |> Chunk.Number

let rec calculateValue2 (chunks: Chunk list)  =
    let addIndexOption = chunks |> List.tryFindIndex(fun x -> x = Chunk.Add)
    if addIndexOption = None then 
        calculateValue1 chunks
    else
        let addIndex = addIndexOption.Value
        let res = (getValue chunks.[addIndex-1]) + (getValue chunks.[addIndex+1]) |> Chunk.Number
        let pre = chunks |> List.indexed |> List.filter(fun (i,_) -> i < (addIndex-1)) |> List.map snd
        let post = chunks |> List.indexed |> List.filter(fun (i,_) -> i > (addIndex+1)) |> List.map snd
        let appended = (List.append (List.append pre [res]) post)
        calculateValue2 appended
    
let rec calc (calculatorFn) (lst: (Chunk*int) list) =
    let maxPrecedence = lst |> List.map snd |> List.max
    if maxPrecedence = 0 then calculatorFn (lst |> List.map fst) else
    let processed = lst |> List.fold(fun acc element ->
                                        let (curr,remaining) = acc
                                        let (chunk,prec) = element
                                        if prec < maxPrecedence then (curr,element::remaining) else
                                        match chunk with
                                            | Chunk.ParenOpen -> (curr, remaining)
                                            | Chunk.ParenClose -> (List.empty<Chunk>, (calculatorFn (curr |> List.rev), prec-1)::remaining)
                                            | Chunk.Mulply -> (chunk::curr,remaining)
                                            | Chunk.Add  -> (chunk::curr,remaining)
                                            | Chunk.Number _  -> (chunk::curr,remaining)
                                            ) (List.empty<Chunk>, List.empty<Chunk*int>)
    calc calculatorFn (processed |> snd |> List.rev)
    

let calculate calculatorFn (chunks:Chunk seq) = 
    let mapOfPrecedence = chunks 
                            |> Seq.fold(fun acc x ->
                                            let (currentPrecedence,precedenceMap) = acc
                                            match x with 
                                                | Chunk.ParenOpen -> (currentPrecedence + 1, currentPrecedence + 1 :: precedenceMap)
                                                | Chunk.ParenClose -> (currentPrecedence - 1, currentPrecedence::precedenceMap)
                                                | _ -> (currentPrecedence, currentPrecedence::precedenceMap)
                                            ) (0, List.empty<int>)
                            |> snd 
                            |> List.rev
    let merged = List.zip (chunks |> Seq.toList) mapOfPrecedence
    calc calculatorFn merged

let mapLine (line:string) =
    let exploded = line.Replace("(","( ").Replace(")"," )").Split(" ")
    exploded |> Seq.map(fun x ->
                            match x with 
                                | ")" -> Chunk.ParenClose
                                | "(" -> Chunk.ParenOpen
                                | "+" -> Chunk.Add
                                | "*" -> Chunk.Mulply
                                | str -> (int64 str) |> Chunk.Number)

let solution1 () =
    let mapped = readLines("data\\18-01.txt") |> Seq.map mapLine
    let results = mapped |> Seq.map((calculate calculateValue1) >> getValue) |> Seq.map(fun x -> int64 x) |> Seq.toArray
    results |> Array.sum

let solution2 () =
    let mapped = readLines("data\\18-01.txt") |> Seq.map mapLine
    let results = mapped |> Seq.map((calculate calculateValue2) >> getValue) |> Seq.map(fun x -> int64 x) |> Seq.toArray
    results |> Array.sum