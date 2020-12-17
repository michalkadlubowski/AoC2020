module day3

open System
open Microsoft.FSharp.Core.Operators.Checked

let [<Literal>] Tree = '#'
let [<Literal>] Free = '.'
type Terrain = Tree | Free

type TerrainGrid = Terrain[,]

let getTerrainAt (grid:TerrainGrid) (right:int) (down:int) =
    let maxLength = Array2D.length2 grid
    let adjustedRight = right % maxLength
    grid.[down,adjustedRight]

let convetToUnon x =
    match x with
        | '#' -> Terrain.Tree
        | '.' -> Terrain.Free
        | _ -> raise <| new ArgumentException("invald terrain")

let loadDataAsTerrainGrid () = 
    let terrainRows = readLines("data\\03-01.txt") |>
                        Seq.map(fun l -> l.ToCharArray() |> Seq.map(convetToUnon) |> Seq.toArray) |>
                        Seq.toArray
    Array2D.init (Seq.length terrainRows) (terrainRows.[0] |> Array.length) (fun x y -> terrainRows.[x].[y])

let path maxIndex right down = seq {
            for i in 1..((maxIndex-1)/down) do
                ((i * right), (i * down))
}

let countTrees right down = 
    let terrainGrid = loadDataAsTerrainGrid ()
    let maxIndex = Array2D.length1 terrainGrid
    path maxIndex right down |> 
        Seq.map(fun point -> getTerrainAt terrainGrid (fst point) (snd point)) |>
        Seq.filter(fun t -> match t with
                            | Tree -> true
                            | _ -> false ) |>
        Seq.length |>
        int64

let solution1 () = 
     countTrees 3 1

let solution2 ()= 
    (countTrees 1 1) * (countTrees 3 1) * (countTrees 5 1) * (countTrees 7 1) * (countTrees 1 2)