module day24

open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type directions = E | SE | SW | W | NW | NE

let toModifier dir =
    match dir with
        | E -> (1,0)
        | SE -> (1,-1)
        | SW -> (0,-1)
        | W -> (-1,0)
        | NW -> (-1,1)
        | NE -> (0,1)

let mapToDirection str = 
    match str with 
        | "e" -> Some E
        | "se" -> Some SE
        | "sw" -> Some SW
        | "w" -> Some W
        | "nw" -> Some NW
        | "ne" -> Some NE
        | _ -> None

let mapLine (line:string) = 
    line |> Seq.fold(fun acc x -> 
                        let (prev, all) = acc
                        let current = prev + x.ToString()
                        let dir = mapToDirection current
                        if dir.IsSome then ("",dir.Value::all)
                        else (current, all)) ("", List.empty<directions>)
         |> snd 
         |> List.rev

let getNeighbours (position:int*int) =
    let (x,y) = position
    [(x + 1, y);(x + 1,y-1);(x,y-1);(x-1,y);(x-1,y+1);(x,y+1)]

let step (initialFlipped:(int*int) list) = 
    let set = initialFlipped |> set
    let relevantTiles =  List.collect getNeighbours initialFlipped
                            |> List.append initialFlipped
                            |> List.map(fun x -> x, (Set.contains x set))
                            |> dict
    let allTiles = relevantTiles.Keys
    let newState = allTiles
                        |> Seq.map(fun tile ->
                                        let isSet = Set.contains tile set
                                        let blackNeighbours = tile |> getNeighbours |> Seq.filter (fun x -> Set.contains x set) |> Seq.length
                                        match (isSet,blackNeighbours) with
                                            | (true,_) when (blackNeighbours = 0 || blackNeighbours > 2) -> (tile,false)
                                            | (true,_) -> (tile,true)
                                            | (false,2) -> (tile,true)
                                            | (false,_) -> (tile,false)
                                        )
    let newFlipped = newState |> Seq.filter(fun x -> snd x) |> Seq.map fst |> Seq.toList
    newFlipped

let rec stepTimes flippedTiles count =
    if count = 0 then flippedTiles
    else stepTimes (step flippedTiles) (count-1)

let getFlippedTilesFromInput () =
    let dirs = readLines "data\\24-01.txt" |> Seq.map mapLine |> Seq.toList
    let modifiers = dirs |> List.map (fun x -> x |> List.map toModifier)
    let destinations = modifiers 
                        |> List.map(fun x -> x |> List.fold(fun acc x ->
                                                            let (accx,accy) = acc
                                                            let (currx,curry) = x
                                                            (accx + currx, accy + curry)) (0,0))
    let byCount = destinations |> List.countBy id
    byCount |> List.filter (fun x -> (snd x)%2 <>0) |> List.map fst

let solution1 () =
    getFlippedTilesFromInput () |> List.length

let solution2 () =
    let initialFlipped = getFlippedTilesFromInput ()
    let res = stepTimes initialFlipped 100
    res |> List.length