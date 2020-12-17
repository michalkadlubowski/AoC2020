module day15

open System
open System.Collections.Generic

let initNums (mem:int[]) (numbers: int array) =
    numbers |> Array.iteri(fun i n -> Array.set mem n (i+1))

let getNumberToSayNext (mem:int[]) (number:int) (turnNo:int) =
        let prevTurnNumOccured = mem.[number]
        let toSay = match prevTurnNumOccured with
                        | 0 -> 0
                        | _ -> turnNo - prevTurnNumOccured
        toSay

let rec speakTimes mem num times turnNo=
    let numberToSayNext = getNumberToSayNext mem num turnNo
    if times = 0 then 
        num
    else
        Array.set mem num (turnNo)
        speakTimes mem numberToSayNext (times-1) ( turnNo + 1)

let solution1 () =
    let nums = readLine("data\\15-01.txt").Split(',') |> Array.map(int)
    let mem = Array.zeroCreate<int> 2020
    initNums mem nums
    speakTimes mem 0 (2020-nums.Length-1) (nums.Length + 1)

let solution2 () =
    let nums = readLine("data\\15-01.txt").Split(',') |> Array.map(int)
    let mem = Array.zeroCreate<int> 30000000
    initNums mem nums
    speakTimes mem 0 (30000000-nums.Length-1) (nums.Length + 1)