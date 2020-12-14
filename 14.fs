module day14

open utils
open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked
open System.Collections.Generic
    
type AocProgram = bool option array * Dictionary<int64,bool[]>

let initProgram () : AocProgram=
    let mask: bool option array = Array.zeroCreate 36
    let mem = new Dictionary<int64,bool[]>()
    (mask, mem)

let strToBitArray (input:string) : bool[] =
    input |> Seq.map(fun c -> if c = '0' then false else true) |> Seq.toArray

let maskToBitArray (input:string) =
    input 
    |> Seq.map(fun c -> 
            if c = 'X' then Option.None
            else Some (if c = '0' then false else true)) 
    |> Seq.toArray

let setMask (mask: bool option array) (program: AocProgram): AocProgram = (mask, snd program)

let convertBitArrayToNumber (arr:bool[])  =
    arr 
        |> Array.mapi(fun i x ->
                        let pow = arr.Length - i - 1
                        let m = if x then 1 else 0
                        float(m)*(float(2)**float(pow)))
        |> Array.sum

let sumAll (program: AocProgram) =
    snd program |> Dictionary.ValueCollection|> Seq.map(convertBitArrayToNumber) |> Seq.sum

let rec intToBinary (i:int64) : string =
    let rem = snd (Math.DivRem(i,int64(2)))
    match i with
            | 0L | 1L -> (string i)
            | _ -> (intToBinary (i / 2L)) + string (rem)

let valueAsBitArr value =
    let valueAsBinaryStr = intToBinary value
    valueAsBinaryStr.PadLeft(36,'0') |> strToBitArray

let applyMaskToAddress (mask: bool option array) (address: int64) =
    let addressBits = valueAsBitArr (int64 address)
    let maskAddressPossibilities = mask |> Array.mapi(fun i x ->
                                            match x with
                                                | None -> [false;true]
                                                | Some true -> [true]
                                                | Some false -> [addressBits.[i]]) |> Array.toList
    let binaryAdressesList =  List.foldBack(fun (addresses:bool list) (acc:(bool list) list)  ->
                                                        if acc.Length = 0 then addresses |> List.map(fun x -> x::[])
                                                        else
                                                            acc |> cartesian addresses
                                                                |> List.map(fun x -> (fst x)::(snd x))
                                                        ) maskAddressPossibilities List.empty<bool list>
    binaryAdressesList |> List.map(fun x -> int64 (convertBitArrayToNumber (x |> List.toArray))) |> Array.ofList

let addValue (value: bool array) (position: int64) (program: AocProgram) =
    let programMask = fst program
    let maskedValue = value |> Array.mapi(fun i x ->
                                            match programMask.[i] with
                                                | None -> x
                                                | Some m -> m)
    if (snd program).ContainsKey(position) then (snd program).Remove(position) |> ignore
    (snd program).Add(position, maskedValue)
    program


let addValue2 (value: bool array) (position: int64) (program: AocProgram) =
    let programMask = fst program
    let positions = applyMaskToAddress programMask position
    positions |> Array.iter(fun p ->
                                let pos = p
                                if (snd program).ContainsKey(pos) then (snd program).Remove(pos) |> ignore
                                (snd program).Add(pos, value))
    program

let mapLine (input:string) addFunc =
    let isMask = input.StartsWith("mask = ")
    if isMask then
        let maskPart = input.[7..]
        let mask = maskToBitArray maskPart
        if maskPart.Length <> 36 then raise <| ArgumentException "wrong mask"
        setMask mask
    else
        let x = Regex.Match(input,@"mem\[(\d+)\] = (\d+)")
        let pos = int64 x.Groups.[1].Value
        let value = int64 x.Groups.[2].Value
        addFunc (valueAsBitArr value) pos

let solution1 () =
    let rows = readLines("data\\14-01.txt") |> Seq.map(fun l -> mapLine l addValue)
    let initProgram = initProgram()
    let resultingProgram = rows |> Seq.fold(fun acc x -> x acc) initProgram
    int64(sumAll resultingProgram)
    
let solution2 () =
    let rows = readLines("data\\14-01.txt") |> Seq.map(fun l -> mapLine l addValue2)
    let initProgram = initProgram ()
    let resultingProgram = rows |> Seq.fold(fun acc x -> x acc) initProgram
    int64(sumAll resultingProgram)