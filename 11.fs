module day11

open System
    
type Tile = OccupiedSeat | EmptySeat | Floor
type TileGrid = Tile[,]
type Position = int*int

let convetToUnon x =
    match x with
        | 'L' -> Tile.EmptySeat
        | '#' -> Tile.OccupiedSeat
        | '.' -> Tile.Floor
        | _ -> raise <| new ArgumentException("invald terrain")


let loadDataAsTileGrid () =
    let terrainRows = readLines("data\\11-01.txt") |>
                        Seq.map(fun l -> l.ToCharArray() |> Seq.map(convetToUnon) |> Seq.toArray) |>
                        Seq.toArray
    Array2D.init (Seq.length terrainRows) (terrainRows.[0] |> Array.length) (fun x y -> terrainRows.[x].[y])

let transformations : (int->Position->Position)[] =
    [|(fun offset (x,y) -> x-offset, y+offset); (fun offset (x,y) -> x,y+offset); (fun offset (x,y) -> x+offset,y+offset); (fun offset (x,y) -> x-offset,y); (fun offset (x,y) -> x+offset,y); (fun offset (x,y) -> x-offset,y-offset) ; (fun offset (x,y) ->  x,y-offset); (fun offset (x,y) -> x+offset,y-offset)|]

let withinBounds (x:int) (y:int) (grid:TileGrid) =
    let width = grid |> Array2D.length1
    let height = grid |> Array2D.length2
    (0 <= x  && x < width) && (0 <= y && y < height)

let getNeighbouringTiles (x:int) (y:int) (grid:TileGrid) : Tile[] =
    transformations |> Array.map(fun transformFun -> transformFun 1 (x,y)) |> Array.filter(fun (a,b) -> (withinBounds a b grid)) |> Array.map (fun x -> grid.[fst x,snd x])

let getSetsOfViewLines (x:int) (y:int) (grid:TileGrid) =
    let mapTransformersToTiles (tranfsormers: (Position->Position)[]) =
        let tilesPositionsInDirection = tranfsormers |> Array.map(fun t -> t(x,y))
        let filteredPositionsInOneDirections = tilesPositionsInDirection |> Array.filter(fun a -> withinBounds (fst a) (snd a) grid)
        let mapped = filteredPositionsInOneDirections |> Array.map (fun x -> grid.[fst x,snd x])
        mapped
    let maxReach = Math.Max ((Array2D.length1 grid), (Array2D.length2 grid))
    let transformationSets =  transformations |> Array.map(fun t -> [1..maxReach] |> Array.ofList |> Array.map(fun offset -> t offset))
    transformationSets |> Array.map(fun e -> mapTransformersToTiles e)

let getVisableTile (tiles: Tile[]) : Tile =
    let firstVisalbe = tiles |> Array.tryFind(fun t -> t = Tile.EmptySeat || t = Tile.OccupiedSeat)
    if Option.isNone firstVisalbe then Tile.Floor else firstVisalbe.Value
    
let tickSeatPart1 (x:int) (y:int) (grid:TileGrid) : Tile =
    let tile = grid.[x,y]
    let neighbours = getNeighbouringTiles x y grid
    let occupiedNeighboursCount = neighbours |> Array.filter(fun t -> t = Tile.OccupiedSeat) |> Array.length
    match tile with
        | Tile.Floor -> Tile.Floor
        | Tile.OccupiedSeat -> if occupiedNeighboursCount >= 4 then Tile.EmptySeat else Tile.OccupiedSeat
        | Tile.EmptySeat -> if occupiedNeighboursCount = 0 then Tile.OccupiedSeat else Tile.EmptySeat

let tickSeatPart2 (x:int) (y:int) (grid:TileGrid) : Tile =
    let tile = grid.[x,y]
    let neighbours = getSetsOfViewLines x y grid |> Array.map(fun x -> getVisableTile x)
    let occupiedNeighboursCount = neighbours |> Array.filter(fun t -> t = Tile.OccupiedSeat) |> Array.length
    match tile with
        | Tile.Floor -> Tile.Floor
        | Tile.OccupiedSeat -> if occupiedNeighboursCount >= 5 then Tile.EmptySeat else Tile.OccupiedSeat
        | Tile.EmptySeat -> if occupiedNeighboursCount = 0 then Tile.OccupiedSeat else Tile.EmptySeat

let tickGrid (grid:TileGrid) tickSeatFn : TileGrid =
    grid |> Array2D.mapi (fun x y _-> tickSeatFn x y grid)

let countOccupied (grid:TileGrid) : int =
    grid |> Seq.cast<Tile> |> Seq.filter(fun x -> x = Tile.OccupiedSeat) |> Seq.length

let rec tickUntilStable (grid:TileGrid) tickSeatFn : TileGrid =
    let newGrid = tickGrid grid tickSeatFn
    let countold = countOccupied grid
    let countNew = countOccupied newGrid
    if countold <> countNew then tickUntilStable newGrid tickSeatFn
    else newGrid
    
let solution1 () =
    let grid = loadDataAsTileGrid()
    let solution = tickUntilStable grid tickSeatPart1
    countOccupied solution
    
let solution2 () =
    let grid = loadDataAsTileGrid()
    let solution = tickUntilStable grid tickSeatPart2
    countOccupied solution