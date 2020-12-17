module day7

open System

type BagInfo = string * Set<int * string>
    
let processLine (line: string) : BagInfo = 
    let part1 = line.Split(" bags contain ")
    let nodeName = part1.[0]
    let rest = part1.[1].Split(',') |>
                Array.map(fun s -> s.Replace(" bags", "").Replace(" bag", ""). Replace(".","").Trim(' ').TrimEnd(' ')) |>
                Array.filter(fun s -> s <> "no other") |>
                Array.map(fun s ->  (Int32.Parse(s.[0..1]), s.[2..]))
    (nodeName, Set.ofArray rest)

let rec countOuter (input: BagInfo[]) (searchItems: string[]) (foundBags: Set<string>) : int=
    let allContaining = input |> 
                            Array.filter(fun bi -> (snd bi) |>
                                Set.map(fun s -> snd s) |>
                                    Set.exists(fun x -> Array.contains x searchItems))
    let newFound = allContaining |> Array.filter(fun x -> (Set.contains (fst x) foundBags) = false)
    let newFoundCount = Array.length newFound
    if newFoundCount = 0 then 
        Seq.length foundBags
    else
        let newSet = newFound |> Array.fold(fun acc e -> Set.add (fst e) acc ) foundBags 
        countOuter input (allContaining |> Array.map(fun x -> fst x)) newSet

let rec countInner (input: BagInfo[]) (searchItems: (int*string)[]) (count: int) : int=
    let newSearch = searchItems |> Array.map(fun si ->
        let item = Array.find (fun x -> fst x = snd si) input
        let newSearches = Set.map(fun c -> (fst si * fst c, snd c)) (snd item)
        (si, newSearches)
    )
    let totalCount =  count + (searchItems |> Array.sumBy(fun i -> fst i))
    let toSearch = newSearch |> Array.filter (fun s -> (snd s |> Set.isEmpty) = false )

    if Array.length toSearch = 0 then 
        totalCount
    else
        let flattenedSearch = toSearch |> Array.map(fun s-> (snd s) |> Set.toArray) |> Array.concat
        countInner input flattenedSearch totalCount

let solution1 () = 
    let mapped = readLines("data\\07-01.txt") |>
                    Seq.map(processLine) |> Array.ofSeq
    countOuter mapped [|"shiny gold"|] Set.empty<string>

let solution2 () = 
    let mapped = readLines("data\\07-01.txt") |>
                    Seq.map(processLine) |> Array.ofSeq
    (countInner mapped [|(1,"shiny gold")|] 0) - 1
