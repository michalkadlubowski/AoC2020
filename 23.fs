module day23

open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic

type cupGame = LinkedListNode<int> * LinkedList<int>

let inline charToInt c = int c - int '0'

let rec getLabelNotInCups (label:int) (cups:int array) totalLength=
    let target = if label = 0 then totalLength  else label
    if Array.contains target cups then
        getLabelNotInCups (target-1) cups totalLength
    else
        target

let nextOrFirst (node:LinkedListNode<int>)=
    match node.Next with
        | null -> node.List.First
        | n -> n
    
let step (game:cupGame) (dict:Dictionary<int,LinkedListNode<int>>) =
    let (currentCup,cupsState) = game
    let currentCupLabel = currentCup.Value
    let next1 = nextOrFirst currentCup
    let next2 = nextOrFirst next1
    let next3 = nextOrFirst next2
    let next4 = nextOrFirst next3
    cupsState.Remove(next1)
    cupsState.Remove(next2)
    cupsState.Remove(next3)
    let destLabel = getLabelNotInCups (currentCup.Value - 1) [|next1.Value;next2.Value;next3.Value|] dict.Count
    let destNode = dict.[destLabel]
    cupsState.AddAfter(destNode, next1)
    cupsState.AddAfter(next1, next2)
    cupsState.AddAfter(next2, next3)
    (next4, cupsState)

let rec stepTimes game times dict =
    if times = 0 then game
    else stepTimes (step game dict) (times-1) dict

let rec fillDict (node:LinkedListNode<int>) (dict:Dictionary<int,LinkedListNode<int>>) =
    if node = null then dict
    else
        dict.Add(node.Value, node)
        fillDict node.Next dict

let solution1 () =
    let nums = readLine "data\\23-01.txt" |> Seq.map (fun x -> charToInt x) |> Seq.toArray
    let list = new LinkedList<int>(nums)
    let firstNode = list.First
    let dict = fillDict firstNode (new Dictionary<int, LinkedListNode<int>>())
    let (_,res) = stepTimes (firstNode,list) 100 dict
    let one = dict.[1]
    let (numsString,_) = [1..8] 
                            |> Seq.fold(fun ((str,acc):string*LinkedListNode<int>) x -> 
                                                let added = str + ((nextOrFirst acc).Value.ToString())
                                                (added,nextOrFirst acc)) ("",one)
    numsString |> int64

let solution2 () =
   let nums = readLine "data\\23-01.txt" |> Seq.map (fun x -> charToInt x) |> Seq.toArray
   let arr = List.init 1000000 (fun index -> if index < 9 then nums.[index] else index+1 ) 
   let list = new LinkedList<int>(arr)
   let firstNode = list.First
   let dict = fillDict firstNode (new Dictionary<int, LinkedListNode<int>>())
   let (_,res) = stepTimes (firstNode,list) 10000000 dict
   let one = dict.[1]
   let plusone = one.Next
   let plustwo = plusone.Next
   (int64 plusone.Value)*(int64 plustwo.Value)