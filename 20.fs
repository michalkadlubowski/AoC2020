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

let getRightColumn tile = 
    let len = tile.Grid |> Array2D.length1
    Array.init len (fun i -> tile.Grid.[i,len-1])

let getLeftColumn tile = 
    let len = tile.Grid |> Array2D.length1
    Array.init len (fun i -> tile.Grid.[i,0]) 

let getTopRow tile = 
    let len = tile.Grid |> Array2D.length1
    Array.init len (fun i -> tile.Grid.[0,i]) 

let getBottomRow tile = 
    let len = tile.Grid |> Array2D.length1
    Array.init len (fun i -> tile.Grid.[len-1,i])             

let matchesOnRight tile1 tile2 = 
    (tile1 |> getRightColumn) = (tile2 |> getLeftColumn)

let matchesOnLeft tile1 tile2 = matchesOnRight tile2 tile1

let matchesOnTop tile1 tile2 =
    (tile1 |> getTopRow) = (tile2 |> getBottomRow)

let matchesOnBottom tile1 tile2 = matchesOnTop tile2 tile1

let find2D item (arr: 'a[,]) = Seq.tryPick id <| seq {
    for i in 0..(arr.GetLength 0 - 1) do
        for j in 0..(arr.GetLength 1 - 1) do
            if arr.[i,j] = item 
                then yield Some (i,j) 
                else yield None
}

let isCellValid (tilesBoard:Tile option[,]) rowNo collNo = 
    // assume left to right top to bottom
    let tile = Array2D.get tilesBoard rowNo collNo
    let tileOnTop = if rowNo>0 then Array2D.get tilesBoard (rowNo-1) collNo else None
    let tileOnLeft = if collNo>0 then Array2D.get tilesBoard rowNo (collNo-1) else None
    if tileOnLeft.IsSome && (matchesOnLeft tile.Value tileOnLeft.Value = false) then false
    elif tileOnTop.IsSome && (matchesOnTop tile.Value tileOnTop.Value = false) then false
    else true

let rec backTrack (possibilities:(int * Tile seq) seq) board = 
        let emptyFieldCoordinates = find2D None board
        if (emptyFieldCoordinates.IsNone) then
            Some board
        else
            let row, col = fst emptyFieldCoordinates.Value, snd emptyFieldCoordinates.Value
            possibilities
            |> Seq.map (fun value -> 
                                (snd value) |> Seq.map (fun tile ->
                                                    board.SetValue(Option.Some tile, row, col)           
                                                    match (isCellValid board row col && backTrack (possibilities|> Seq.filter(fun x -> fst x <> fst value)) board |> Option.isSome) with
                                                    | true  -> Some board
                                                    | false -> 
                                                        board.SetValue(None, row, col)
                                                        None) |> Seq.tryPick id
            ) |> Seq.tryPick id     

let solve (possibilities:(int * Tile seq) seq)=
    let resolution = possibilities |> Seq.length |> float |> Math.Sqrt |> int
    let tileArray = Array2D.zeroCreate<Tile option> resolution resolution
    let res = backTrack possibilities tileArray
    // let findMatching = possibilities |> 
    let topLeft = (Array2D.get res.Value 0 0 |> Option.get).Number |> int64
    let topRight = (Array2D.get res.Value 0 (resolution-1) |> Option.get).Number |> int64
    let bottomLeft = (Array2D.get res.Value (resolution-1) 0 |> Option.get).Number |> int64
    let bottomRight = (Array2D.get res.Value (resolution-1) (resolution-1) |> Option.get).Number |> int64
    topLeft * topRight * bottomLeft * bottomRight

let printTile (tile:Tile) = 
    printfn "%s" ""
    for r = 0 to Array2D.length1 tile.Grid - 1 do
        printfn "%s" ""
        for c = 0 to Array2D.length2 tile.Grid - 1 do
            printf "%c" tile.Grid.[r, c]

let solution1 () =
    let tiles = getMappedInput()
    let possibilities = getMappedInput() |> Array.map(fun x -> (x.Number,mapTilesToPossibilities x))
    let res = solve possibilities
    res

let solution2 () =
    0