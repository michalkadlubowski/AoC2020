module day13

open System
            
let mapToSchedule (lines:string[]) =
    let time = Int32.Parse lines.[0]
    let ids = lines.[1].Split ',' |> Seq.filter(fun x -> x <> "x") |> Seq.map(Int32.Parse) |> Seq.toArray
    (time, ids)

let mapToTimesWithOffsets (lines:string[]) =
    lines.[1].Split ',' |> Array.mapi(fun i x -> (x,i)) |>
        Array.filter(fun x -> fst x <> "x") |> Array.map(fun x -> (Int32.Parse (fst x), snd x)) 

let solution1 () =
    let data = readLines("data\\13-01.txt") |> Seq.toArray |> mapToSchedule
    let (myTime, ids) = data
    let closest = ids |> Array.minBy(fun d -> ((myTime/d)+1)* d)
    closest * (closest-(myTime % closest))

let rec solve2 (data:(int64*int64)[]) (ts:int64) (lcm:int64) : int64 = 
    let (id,start) = data.[0]
    let timeSeq = Seq.initInfinite(fun i -> ts + int64 i * lcm) |> Seq.skip(1)
    let nextMatchingTs = timeSeq |> Seq.find(fun i -> (snd (Math.DivRem((i+start), id)) = 0L))
    let nextLcm = lcm*id
    if ((Array.length data) = 1) then nextMatchingTs else  solve2 data.[1..] nextMatchingTs nextLcm

let solution2 () =
    let data = readLines("data\\13-01.txt") |> Seq.toArray |>
                 mapToTimesWithOffsets |> Array.map(fun x -> (int64 (fst x), int64 (snd x)))
    solve2 data 0L 1L
