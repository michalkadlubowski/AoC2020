module day4

open System
open System.Text.RegularExpressions

type PassportInfo = string * string
type Document = PassportInfo[]

let mapToInfo (input: string) : PassportInfo =
    let pairs = input.Split ':'
    (pairs.[0], pairs.[1])
    
let getProps (input: string) = 
    input.Split ' ' |>
    Array.map(mapToInfo)

let mapLinesToProps (input: seq<string>) : Document =
    input |>
        Seq.collect(getProps) |> Seq.toArray

let valdateBetweenValues (input:string) (min:int) (max:int)=
     match Int32.TryParse input with
        | true, value -> min <= value && value <= max
        | false, _ -> false

let mapInput (input: seq<string>) : seq<Document> = 
    input |>
        splitSeq (fun x-> x = "") |>
        Seq.map(mapLinesToProps)

let validateByr (input:string) =
    valdateBetweenValues input 1920 2002

let validateIyr(input:string) =
    valdateBetweenValues input 2010 2020

let validateEyr (input:string) =
    valdateBetweenValues input 2020 2030

let validateInchHeight (input:string) =
    valdateBetweenValues input 59 76

let validateCmHeight (input:string) =
    valdateBetweenValues input 150 193

let validateHeight (heightVal: string) (unit:string) =
    match unit with
        | "cm" -> validateCmHeight heightVal
        | "in" -> validateInchHeight heightVal
        | _ -> false

let validateHgt (input:string) =
    let x = Regex.Match(input,@"(\d{1,3})(in|cm)")
    match x.Success with
        | true -> validateHeight x.Groups.[1].Value x.Groups.[2].Value
        | false -> false

let validateHcl input =
    Regex.IsMatch (input, @"^#(\d|[a-f]){6}$")

let validateEcl (input:string) =
    [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|] |>
    Array.contains input
    
let validatePid (input:string) =
    Regex.IsMatch (input, @"^\d{9}$")

let validatePassportInfo (input:PassportInfo) =
    match fst input with
        | "byr" -> validateByr <| snd input
        | "iyr" -> validateIyr <| snd input
        | "eyr" -> validateEyr <| snd input
        | "hgt" -> validateHgt <| snd input
        | "hcl" -> validateHcl <| snd input
        | "ecl" -> validateEcl <| snd input
        | "pid" -> validatePid <| snd input
        | _ -> true

let isValid (doc: Document) =
    let req = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]
    let keys = doc |> Array.map(fun d -> fst d)
    req |> Array.fold (fun acc x -> acc && Array.contains x keys) true 

let isValid2 (doc: Document) =
    let validDocInfos = doc |> Array.filter(fun di -> validatePassportInfo di) 
    isValid validDocInfos

let solution1 () = 
    readLines("data\\04-01.txt") |>
         mapInput |> Seq.filter(isValid) |> Seq.length

let solution2 () = 
    readLines("data\\04-01.txt") |>
         mapInput |> Seq.filter(isValid2) |> Seq.length