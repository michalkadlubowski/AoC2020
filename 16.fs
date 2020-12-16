module day16

open utils
open System
open System.Collections.Generic
open System.Text.RegularExpressions

type Ticket = int array
type Requirement = { Min:int; Max: int }
type Rule = { Name:string; Constrains: Requirement * Requirement}

let mapToTicket (nums : string array) : Ticket = nums |> Array.map int

let parseDefinition input =
    let x = Regex.Match(input,@"(.+)\: (\d+)-(\d+) or (\d+)-(\d+)")
    let req1 = { Min = (int x.Groups.[2].Value); Max = (int x.Groups.[3].Value) }
    let req2 = { Min = (int x.Groups.[4].Value); Max = (int x.Groups.[5].Value) }
    { Name = x.Groups.[1].Value; Constrains = (req1, req2)}

let validateAgainstRule (rule:Rule) value =
    ((fst rule.Constrains).Min <= value && (fst rule.Constrains).Max >= value) ||
    ((snd rule.Constrains).Min <= value && (snd rule.Constrains).Max >= value)

let valudateAllRules definitions =
    definitions |> Array.fold(fun (acc:int->bool) x -> fun z -> (acc z) || (validateAgainstRule x z)) (fun x -> false)

let rec mapFieldsValuesToRules (definitions: Rule array) (valuesByIndex:int[][]) (mapped:(int*string) array)=
    if(definitions.Length = 0) then
        mapped
    else
        let alreadyFound = mapped |> Array.map(fst) |> HashSet
        let ruleValuesCheckers = definitions |> Array.map(fun d -> (d,fun (i:int[]) -> Array.forall(validateAgainstRule d) i))
        let indexOfValuesThatMatchOneRule =
            valuesByIndex
                |> Array.indexed
                |> Array.find(fun (i,values) ->
                    alreadyFound.Contains(i) = false &&
                    (ruleValuesCheckers |> Array.filter(fun rc -> (snd rc) values)).Length = 1)
                |> fst

        let ruleThatMatches = ruleValuesCheckers |> Array.find(fun rc -> (snd rc) valuesByIndex.[indexOfValuesThatMatchOneRule])
        let pair = (indexOfValuesThatMatchOneRule, (fst ruleThatMatches).Name)
        let remainingRules = ruleValuesCheckers |> Array.filter(fun r -> (fst r).Name <> (fst ruleThatMatches).Name) |> Array.map(fst)
        let results = mapped |> Array.append [|pair|]
        mapFieldsValuesToRules remainingRules valuesByIndex results

let mapFieldsToRules (definitions: Rule array) (tickets:Ticket array) =
    let maxIndex = tickets.[0].Length-1
    let fieldValuesByIndex = [|0..maxIndex|] |> Array.map(fun x -> tickets |> Array.map(fun t -> Array.get t x))
    mapFieldsValuesToRules definitions fieldValuesByIndex Array.empty<int*string>

let solution1 () =
    let split = readLines("data\\16-01.txt") |> utils.splitSeq(fun s -> s = "") |> Seq.map(Seq.toArray) |> Seq.toArray
    let definitions = split.[0] |> Array.map parseDefinition
    let nearbyTickets = split.[2].[1..] |> Array.map(fun x ->  mapToTicket (x.Split(',')))
    let validateField = valudateAllRules definitions
    let invalidFields = nearbyTickets |> Array.map (Array.filter (fun x -> validateField x = false)) |> Array.concat
    Array.sum invalidFields

let solution2 () =
    let split = readLines("data\\16-01.txt") |> utils.splitSeq(fun s -> s = "") |> Seq.map(Seq.toArray) |> Seq.toArray
    let definitions = split.[0] |> Array.map parseDefinition
    let myTicket = split.[1].[1].Split(',') |>  mapToTicket
    let nearbyTickets = split.[2].[1..] |> Array.map(fun x ->  mapToTicket (x.Split(',')))
    let validateField = valudateAllRules definitions
    let validNerabyTickets = nearbyTickets |> Array.filter(Array.forall(validateField))
    let res = mapFieldsToRules definitions validNerabyTickets
    let result = res
                    |> Array.filter(fun r -> (snd r).StartsWith("departure"))
                    |> Array.map fst
                    |> Array.map(fun x -> myTicket.[x])
                    |> Array.fold(fun acc x -> acc * (int64 x)) 1L
    result