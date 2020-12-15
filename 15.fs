module day15

open utils
open System
open System.Collections.Generic

let initDict (mem:Dictionary<int,List<int>>) (numbers: int array) =
    numbers |> Array.iteri(fun i n ->
        let list = new List<int>()
        list.Add(i+1)
        mem.Add(n, list))
    numbers |> Array.last

let getLrus (mem:Dictionary<int,List<int>>) num =
    if mem.ContainsKey(num) && mem.[num].Count > 1 then
        let len = mem.[num].Count
        (mem.[num].[len-1], mem.[num].[len-2])
    else
        (0,0)

let speakNumber (mem:Dictionary<int,List<int>>) (prevNumber:int) (turnNo:int) =
    let addToMem toSay =
        if mem.ContainsKey(toSay) then
            mem.[toSay].Add(turnNo)
        else
            let list = new List<int>()
            list.Add(turnNo)
            mem.Add(toSay,list)

    if mem.ContainsKey(prevNumber) && mem.[prevNumber].Count > 1 then
        let len = mem.[prevNumber].Count
        let toSay = mem.[prevNumber].[len-1] - mem.[prevNumber].[len-2]
        addToMem toSay
        toSay
    else
        let toSay = 0
        addToMem toSay
        toSay

let rec speakTimes mem num times turnNo=
    if times = 0 then num
    else
        let next = speakNumber mem num turnNo
        speakTimes mem next (times-1) ( turnNo + 1)

let solution1 () =
    let nums = readLine("data\\15-01.txt").Split(',') |> Array.map(Int32.Parse)
    let mem = new Dictionary<int,List<int>>()
    let afterInit = initDict mem nums
    speakTimes mem afterInit (2020-nums.Length) (nums.Length + 1)

let solution2 () =
    let nums = readLine("data\\15-01.txt").Split(',') |> Array.map(Int32.Parse)
    let mem = new Dictionary<int,List<int>>()
    let afterInit = initDict mem nums
    speakTimes mem afterInit (30000000-nums.Length) (nums.Length + 1)