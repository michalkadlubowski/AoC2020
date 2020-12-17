module day9

open System
    
let isValid  (countToCheck: int) (existingNumbers: Int64[]) (numberToAdd: Int64): bool =
    let existingLength = Array.length existingNumbers
    if existingLength < countToCheck then true 
    else
        let numsToCheck = existingNumbers.[existingLength-countToCheck..]
        let possibilities = numsToCheck |> Array.map(fun i -> numberToAdd - i) |> Set.ofArray
        numsToCheck |> Array.filter(fun n -> Set.contains n possibilities) |> Array.length > 1

let rec addinngUpToFromBack (numbers: Int64[]) (target: Int64) (acc: Int64[]) : Int64[] option =
    let len = Array.length numbers
    let lastNum = numbers.[len-1]
    let newChain = Array.append [|lastNum|] acc
    if lastNum = target then
        Some(newChain)
    elif lastNum > target then
        None
    elif len = 1 then
        None
    else 
        addinngUpToFromBack numbers.[0..len-2] (target-lastNum) newChain

let findKey (existingNumbers: Int64[]) (targetValue: Int64) : Int64=
    let indexOfTarget = Array.findIndex(fun x -> x = targetValue) existingNumbers
    let indexOfEnd = [0..(indexOfTarget-1)] |> Seq.findBack (fun i -> 
        (addinngUpToFromBack existingNumbers.[0..i] targetValue Array.empty<Int64>).IsSome) 
    let res = addinngUpToFromBack existingNumbers.[0..indexOfEnd] targetValue Array.empty<Int64>
    match res with
        | Some r -> (Array.min r) + (Array.max r)
        | None ->  raise <| new ArgumentException("error")

let solution1 () : Int64 = 
    let validityCheck = isValid 25
    let x = readLines("data\\09-01.txt") |> Seq.map(Int64.Parse) |>
                Seq.fold(fun acc e -> 
                    let isValid = validityCheck (acc |> Array.map(fun x -> fst x)) e
                    Array.append acc [|(e, isValid)|])
                    Array.empty<Int64*bool> |>
                    Seq.find (fun x -> (snd x) = false)
    fst x
    
let solution2 () = 
    let input = readLines("data\\09-01.txt") |> Seq.map(Int64.Parse) |> Array.ofSeq
    findKey input (solution1())