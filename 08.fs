module day8

open utils
open System

let [<Literal>] NOP = "nop"
let [<Literal>] ACC = "acc"
let [<Literal>] JMP = "jmp"
type Op = NOP | ACC | JMP
type Instruction = Op * int
    
let processLine (line: string) : Instruction = 
    let part1 = line.Split(" ")
    let argument = Int32.Parse(part1.[1].Trim('+'))
    match part1.[0] with
        | "nop" -> (NOP,argument)
        | "acc" -> (ACC,argument)
        | "jmp" -> (JMP, argument)
        | _ ->  raise <| new ArgumentException("invald op")

let flipOp (instructions: Instruction[]) (position:int) : Instruction[] =
    let oldInstruction = instructions.[position]
    let newInstruction = match (fst oldInstruction) with 
                            | NOP -> (JMP, snd oldInstruction)
                            | JMP -> (NOP, snd oldInstruction)
                            | _ ->  raise <| new ArgumentException("invald op")
    let newInstructions = [0..((Array.length instructions)-1)] |> 
                            Seq.map (fun i -> if i = position then newInstruction else instructions.[i]) |>
                            Seq.toArray
    newInstructions                  

let rec findLoopOrTermination (instructions: Instruction[]) (accumulator:int) (currentPosition: int) (visited: Set<int>) : int * bool =
    let positionAlreadyVisited = Set.contains currentPosition visited
    if positionAlreadyVisited then (accumulator,false)
    else if currentPosition > (Array.length instructions - 1) then (accumulator,true)
    else    
        let (op,arg) = instructions.[currentPosition]
        let (nextPosition, accValue) = match op with
                                        | ACC -> (currentPosition+1, accumulator + arg)
                                        | JMP -> (currentPosition+arg,accumulator)
                                        | NOP -> (currentPosition+1, accumulator)
        findLoopOrTermination instructions accValue nextPosition (Set.add currentPosition visited)

let findNotLooping (instructions: Instruction[]) : int = 
    let indexesOfPossibleOps = [0..((Array.length instructions)-1)] |> 
                Seq.map (fun i ->
                            let instruction = instructions.[i]
                            match fst instruction with
                                | NOP -> i
                                | JMP -> i
                                | _ -> -1) |>
                Seq.filter(fun i -> i <> -1)

    let indexToSwitch = indexesOfPossibleOps |> 
                    Seq.find(fun i ->
                                    let newInstructions = flipOp instructions i
                                    let res = findLoopOrTermination newInstructions 0 0 Set.empty<int>
                                    snd res
                                )
    indexToSwitch

let solution1 () = 
    let mapped = readLines("data\\08-01.txt") |>
                    Seq.map(processLine) |> Array.ofSeq
    fst (findLoopOrTermination mapped 0 0 Set.empty<int>)

let solution2 () = 
    let mapped = readLines("data\\08-01.txt") |>
                    Seq.map(processLine) |> Array.ofSeq
    let opToSwitch = findNotLooping mapped
    let switchedInstructions = flipOp mapped opToSwitch
    fst (findLoopOrTermination switchedInstructions 0 0 Set.empty<int>)
