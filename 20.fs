module day20

open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open FSharp.Collections.ParallelSeq

type Tile = { Number:int ; Grid:char[,]}

let mapTile (lines:string array) =
    let tileName = lines.[0]
    let number = int (tileName.Substring(5,4))
    let grid = Array2D.init 10 10 (fun x y -> lines.[y+1].[x])
    { Number = number; Grid = grid}

let getMappedInput () = 
    let mapped = readLines("data\\20-01.txt")
                    |> splitSeq(fun s -> s = "")
                    |> Seq.toArray
                    |> Array.map (fun x -> mapTile (x |> Seq.toArray))
    mapped

let rotateTile (tile:Tile) =
    tile

let flipTile tile = 
    let oldArray = tile.Grid
    let newArray = Array2D.init (oldArray |> Array2D.length2) (oldArray |> Array2D.length1) (fun r c -> oldArray.[c,r])
    { Number = tile.Number; Grid = newArray }

let mapTilesToPossibilities tile =
    seq {
        let rotated1 = rotateTile tile
        yield rotated1
        let rotated2 = rotateTile tile
        yield rotated2
        let rotated3 = rotateTile tile
        yield rotated3
        //let flipped = flipTile tile
        
    }

let backTrack tilesMappes tilesToomap =
    0


let solution1 () =
    let tiles = getMappedInput()
    0

let solution2 () =
    0