module day10

open utils
open System

let getPossibilitiesFromPosition (position: int) (joltages: int[]): (int[])=    
    let maxIndex = (Array.length joltages) - 1
    let currentPosJolts = joltages.[position]
    let plus1possible = if maxIndex > (position + 1) then joltages.[position+1] <= currentPosJolts + 3 else false
    let plus2possible = if maxIndex > (position + 2) then joltages.[position+2] <= currentPosJolts + 3 else false
    let plus3possible = if maxIndex > (position + 3) then joltages.[position+3] <= currentPosJolts + 3 else false
    if not plus1possible then [||]
    elif not plus2possible then [|(position+1)|]
    elif not plus3possible then [|(position+1);(position+2)|]
    else [|(position+1);(position+2);(position+3)|]

let solution1 () : int = 
    let input = readLines("data\\10-01.txt") |> Seq.map(Int32.Parse) |> Seq.toArray 
    let out = (input |> Array.max) + 3
    let diffs = Array.append [|0; out|] input |> Array.sort |> Array.pairwise |> Array.map (fun x -> snd x - fst x)
    let countOfDiff1 =  diffs |> Array.filter (fun x -> x = 1) |> Array.length   
    let countOfDiff3 = diffs |> Array.filter (fun x -> x = 3) |> Array.length
    countOfDiff1 * (countOfDiff3)

let solution2 () = 
    let input = readLines("data\\10-01.txt") |> Seq.map(Int32.Parse) |> Seq.toArray
    let out = (input |> Array.max) + 3
    let inputWithInOut = input |> Array.append [|0; out|] |>  Array.sort
    let possibilitiesFromPositon = inputWithInOut |> Array.mapi(fun i _-> getPossibilitiesFromPosition i inputWithInOut)
    let possibilitiesArray = Array.foldBack (fun x (acc : int64 list)-> 
                let indexOfElement = Array.IndexOf(possibilitiesFromPositon,x)
                let calcRelativeIndex = fun i -> i - indexOfElement - 1
                let countOfPossibilities = x |> Array.sumBy(fun e -> acc.[calcRelativeIndex e])
                let nextElementIndex =  calcRelativeIndex (indexOfElement+1)
                let adjusted = 
                    if nextElementIndex < 0 || (nextElementIndex + 1) > (List.length acc) then 
                        1L
                    elif countOfPossibilities <= 1L then
                        acc.[0] 
                    else countOfPossibilities
                adjusted::acc) possibilitiesFromPositon (List.empty<int64>)
    possibilitiesArray.[0]
