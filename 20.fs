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
    let oldArray = tile.Grid
    let height, width = Array2D.length1 oldArray, Array2D.length2 oldArray
    let newGird = Array2D.init width height (fun row column -> Array2D.get oldArray (height - column - 1) row)
    { Number = tile.Number; Grid = newGird }
    
let flipTile tile = 
    let oldArray = tile.Grid
    let len = tile.Grid |> Array2D.length1
    let maxIndex = len-1
    let newArray = Array2D.init (oldArray |> Array2D.length2) (oldArray |> Array2D.length1) (fun r c -> oldArray.[r,maxIndex-c])
    { Number = tile.Number; Grid = newArray }

let mapTilesToPossibilities tile =
    seq {
        yield tile
        let rotated1 = rotateTile tile
        yield rotated1
        let rotated2 = rotateTile rotated1
        yield rotated2
        let rotated3 = rotateTile rotated2
        yield rotated3
        let flipped = flipTile tile
        yield flipped
        let flippedRotated1 = rotateTile flipped
        yield flippedRotated1
        let flippedRotated2 = rotateTile flippedRotated1
        yield flippedRotated2
        let flippedRotated3 = rotateTile flippedRotated2
        yield flippedRotated3
    }

let backTrack (possibilities:(int * Tile seq) seq) =
    // TODO
    0

let printTile (tile:Tile) = 
    printfn "%s" ""
    for r = 0 to Array2D.length1 tile.Grid - 1 do
        printfn "%s" ""
        for c = 0 to Array2D.length2 tile.Grid - 1 do
            printf "%c" tile.Grid.[r, c]

let solution1 () =
    let possibilities = getMappedInput() |> Array.map(fun x -> (x.Number,mapTilesToPossibilities x))
    let res = backTrack possibilities
    res

let solution2 () =
    0