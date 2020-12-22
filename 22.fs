module day22

open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
open Queue

let toString q =
    match q with
    |   Queue(fs, bs) -> List.rev fs |> List.append bs |> List.map string |> List.fold (fun acc x -> acc + "," + x ) ""

let getMapped () =
    let mapped = readLines("data\\22-01.txt") |> splitSeq(fun x -> x = "") |> Seq.toArray
    let player1cards = mapped.[0] |> Seq.skip 1 |> Seq.map int |> Seq.toArray
    let player2cards = mapped.[1] |> Seq.skip 1  |> Seq.map int |> Seq.toArray
    (player1cards, player2cards)

let playRoundv1 p1Cards p2Cards =
    let (p1card,p1q) = p1Cards |> Queue.dequeue
    let (p2card,p2q) = p2Cards |> Queue.dequeue
    if p1card > p2card then
        (p1q |> Queue.enqueue p1card |> Queue.enqueue p2card, p2q)
    else
        ( p1q, p2q |> Queue.enqueue p2card |> Queue.enqueue p1card)

let rec playUntilEnd p1q p2q =
    if p1q |> Queue.isempty then p2q
    elif p2q |> Queue.isempty then p1q
    else
        let (q1,q2) =  playRoundv1 p1q p2q
        playUntilEnd q1 q2
       
let rec playUntilEnd2 (p1q) (p2q) (history:HashSet<string>) =
    if p1q |> Queue.isempty then (2,p1q,p2q)
    elif p2q |> Queue.isempty then (1,p1q,p2q)
    else
        let p2state = toString p2q
        let p1state = toString p1q
        let globalState = p1state + "|" + p2state
        if history.Add(globalState) = false then
            (1,p1q,p2q)
        else
            let (p1card,remainingP1q) = p1q |> Queue.dequeue
            let (p2card,remainingP2q) = p2q |> Queue.dequeue
            if remainingP1q |> Queue.length >= p1card && remainingP2q |> Queue.length >= p2card then
                let p1Cards = [1..p1card] |> Seq.fold(fun acc x ->
                                                            let (acc,q) = acc
                                                            let (element, rest) = Queue.dequeue q
                                                            (acc |> Queue.enqueue element, rest) ) (Queue.empty, remainingP1q)
                                          |> fst
                let p2Cards = [1..p2card] |> Seq.fold(fun acc x ->
                                                            let (acc,q) = acc
                                                            let (element, rest) = Queue.dequeue q
                                                            (acc |> Queue.enqueue element, rest) ) (Queue.empty, remainingP2q)
                                            |> fst
                let (winner, np1,np2) = playUntilEnd2 p1Cards p2Cards (new HashSet<string>())
                match winner with
                    | 1 ->
                            let newp1q = remainingP1q |> Queue.enqueue p1card |> Queue.enqueue p2card
                            playUntilEnd2 newp1q remainingP2q history
                    | 2 ->
                            let newp2q =  remainingP2q |> Queue.enqueue p2card |> Queue.enqueue p1card
                            playUntilEnd2 remainingP1q newp2q history
            else
                if p1card > p2card then
                    let newp1q = remainingP1q |> Queue.enqueue p1card |> Queue.enqueue p2card
                    playUntilEnd2 newp1q remainingP2q history
                else
                    let newp2q =  remainingP2q |> Queue.enqueue p2card |> Queue.enqueue p1card
                    playUntilEnd2 remainingP1q newp2q history
    
let calculateScore q =
    let res = [1..Queue.length q]
                     |> Seq.rev
                     |> Seq.fold(fun (acc:int*queue<int>) (x:int) ->
                                     let (score, q) = acc
                                     let (elem,queue) = Queue.dequeue q
                                     ((score + elem * x), queue)) (0,q)
    fst res

let solution1 () =
    let (p1Cards,p2Cards) = getMapped ()
    let p1q = p1Cards |> Array.fold(fun acc x -> Queue.enqueue x acc) Queue.empty
    let p2q = p2Cards |> Array.fold(fun acc x -> Queue.enqueue x acc) Queue.empty
    let res = playUntilEnd p1q p2q
    calculateScore res

let solution2 () =
    let (p1Cards,p2Cards) = getMapped ()
    let p1q = p1Cards |> Array.fold(fun acc x -> Queue.enqueue x acc) Queue.empty
    let p2q = p2Cards |> Array.fold(fun acc x -> Queue.enqueue x acc) Queue.empty
    let (p,p1,p2) = playUntilEnd2 p1q p2q (new HashSet<string>())
    match p with
        | 1  -> calculateScore p1
        | 2  -> calculateScore p2