module day17

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let mapTo4DArray (lines:string array) =
    let countOfLines = lines.Length
    let lineLength = lines.[0].Length
    let array = Array4D.create<bool> lineLength countOfLines 1 1 false
    lines |> Seq.iteri(fun lineIndex line ->
           line|> Seq.iteri(fun charIndex c ->
                                    let active = match c with
                                                            | '.' -> false
                                                            | '#' -> true
                                                            | _ -> raise <| new ArgumentException("invalidInput")
                                    Array4D.set array charIndex lineIndex 0 0 active))
    array

let mapTo3DArray (lines:string array) =
    let countOfLines = lines.Length
    let lineLength = lines.[0].Length
    let array = Array3D.create<bool> lineLength countOfLines 1 false
    lines |> Seq.iteri(fun lineIndex line ->
           line|> Seq.iteri(fun charIndex c ->
                                                let active = match c with
                                                                        | '.' -> false
                                                                        | '#' -> true
                                                                        | _ -> raise <| new ArgumentException("invalidInput")
                                                Array3D.set array charIndex lineIndex 0 active))
    array    

let transFormationsForPoint3D x y z =
    let xyz = cartesianLstLst [[x-1;x;x+1];[y-1;y;y+1];[z-1;z;z+1]]
    let points = xyz |> List.map(fun xyz -> (xyz.[0],xyz.[1],xyz.[2]))
    points |> List.filter(fun (x1,y1,z1) -> (x1=x && y1=y && z1=z) = false)

let transFormationsForPoint4D x y z w=
    let xyz = cartesianLstLst [[x-1;x;x+1];[y-1;y;y+1];[z-1;z;z+1];[w-1;w;w+1]]
    let points = xyz |> List.map(fun xyz -> (xyz.[0],xyz.[1],xyz.[2],xyz.[3]))
    points |> List.filter(fun (x1,y1,z1,w1) -> (x1=x && y1=y && z1=z && w1=w) = false)

let calculateState4D (prevState: bool[,,,]) x y z w=
    let shiftedX = x-1
    let shiftedY = y-1
    let shiftedZ = z-1
    let shiftedW = w-1
    let isInBounds x y z w =
        x >= 0 && x < (Array4D.length1 prevState) &&
        y >= 0 && y < (Array4D.length2 prevState) &&
        z >= 0 && z < (Array4D.length3 prevState) &&
        w >= 0 && w < (Array4D.length4 prevState)
    let relevantPoints = transFormationsForPoint4D shiftedX shiftedY shiftedZ shiftedW
    let setNeighbours = relevantPoints |> List.map(fun (x,y,z,w) ->
                                            match isInBounds x y z w with
                                                | true -> Array4D.get prevState x y z w
                                                | false -> false
                                            )
    let setNeighboursCount = setNeighbours |> List.filter (fun x -> x)|> List.length
    let prevStateOfCell = isInBounds shiftedX shiftedY shiftedZ shiftedW && Array4D.get prevState shiftedX shiftedY shiftedZ shiftedW
    match prevStateOfCell with
        | true -> setNeighboursCount = 2 || setNeighboursCount = 3
        | false -> setNeighboursCount = 3

let calculateState3D (prevState: bool[,,]) x y z =
    let shiftedX = x-1
    let shiftedY = y-1
    let shiftedZ = z-1
    let isInBounds x y z =
        x >= 0 && x < (Array3D.length1 prevState) &&
        y >= 0 && y < (Array3D.length2 prevState) &&
        z >= 0 && z < (Array3D.length3 prevState)
    let relevantPoints = transFormationsForPoint3D shiftedX shiftedY shiftedZ
    let setNeighbours = relevantPoints |> List.map(fun (x,y,z) ->
                                            match isInBounds x y z with
                                                | true -> Array3D.get prevState x y z
                                                | false -> false
                                            )
    let setNeighboursCount = setNeighbours |> List.filter (fun x -> x)|> List.length
    let prevStateOfCell = isInBounds shiftedX shiftedY shiftedZ && Array3D.get prevState shiftedX shiftedY shiftedZ
    match prevStateOfCell with
        | true -> setNeighboursCount = 2 || setNeighboursCount = 3
        | false -> setNeighboursCount = 3        

let iter4d f (arr: bool[,,,]) =
    for w = 0 to Array4D.length4 arr - 1 do
        for z = 0 to Array4D.length3 arr - 1 do
            for y = 0 to Array4D.length2 arr - 1 do
                for x = 0 to Array4D.length1 arr - 1 do
                    f x y z w

let generateNewState4D (prevState: bool[,,,]) =
    let xDim = Array4D.length1 prevState
    let yDim = Array4D.length2 prevState
    let zDims = Array4D.length3 prevState
    let wDims = Array4D.length4 prevState
    let newStateArr = Array4D.create<bool> (xDim+2) (yDim+2) (zDims+2) (wDims+2) false
    newStateArr |> iter4d (fun x y z w  ->
                            let newState = calculateState4D prevState x y z w
                            Array4D.set newStateArr x y z w newState)
    newStateArr

let generateNewState3D (prevState: bool[,,]) =
    let xDim = Array3D.length1 prevState
    let yDim = Array3D.length2 prevState
    let zDims = Array3D.length3 prevState
    let newStateArr = Array3D.create<bool> (xDim+2) (yDim+2) (zDims+2) false
    newStateArr |> Array3D.iteri(fun x y z e ->
        let newState = calculateState3D prevState x y z
        Array3D.set newStateArr x y z newState)
    newStateArr    

let rec stepBy4D (prevState: bool[,,,]) (noSteps:int) =
    if noSteps = 0 then prevState
    else stepBy4D (generateNewState4D prevState) (noSteps - 1)

let rec stepBy3D (prevState: bool[,,]) (noSteps:int) =
    if noSteps = 0 then prevState
    else stepBy3D (generateNewState3D prevState) (noSteps - 1)    

let solution1 () =
    let mapped3d = readLines("data\\17-01.txt") |> Seq.toArray |> mapTo3DArray
    let finalState = stepBy3D mapped3d 6
    let mutable sum = 0
    Array3D.iteri (fun x y z value->
                    let value = if value = true then 1 else 0
                    sum <- sum + value) finalState
    sum

let solution2 () =
    let mapped4d = readLines("data\\17-01.txt") |> Seq.toArray |> mapTo4DArray
    let test = stepBy4D mapped4d 6
    let mutable sum = 0
    iter4d (fun x y z w->
                    let x = Array4D.get test x y z w
                    let value =if x then 1 else 0
                    sum <- sum + value) test
    sum