module day19

open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

type RuleType = 
 | RuleLinks of int list
 | RuleValue of char 
 | AlternativeLinks of ((int list) * (int list))
type Rule = { Number:int ; Rule:RuleType}

let ruleValRegex = "(\\d+): \"(\\w)\""
let ruleLinkMutli = @"(\d+): ((\d+ ?)+)\| ((\d+ ?)+)"
let ruleLinkSingle = @"(\d+): ((\d+ ?)+)"

let rec ruleToStrings (rules: IDictionary<int,Rule>) (r:Rule)  (currentDepth: int) (filter:string option) =
    let filterFn (x:string) = filter.IsNone || filter.Value.Contains(x)
    let mapRuleLink l dephModifier = l 
                                        |> Seq.map(fun l -> rules.[l]) 
                                        |> Seq.map(fun r -> ruleToStrings rules r (currentDepth + dephModifier) filter )
                                        |> productn
                                        |> Seq.map(fun x -> x |> Seq.fold(fun acc x -> acc + x) "")
                                        |> Seq.filter filterFn
    seq { 
        match r.Rule with 
        | RuleType.RuleValue v -> yield v.ToString()
        | RuleType.RuleLinks l -> yield! mapRuleLink l 1
        | RuleType.AlternativeLinks (l1,l2) -> 
                                  yield! mapRuleLink l1 1
                                  yield! mapRuleLink l2 1
    }

let parseRuleLine line : Rule= 
    if Regex.IsMatch(line, ruleValRegex) then
        let m = Regex.Match(line, ruleValRegex)
        let ruleNo = m.Groups.[1].Value |> int
        let char = m.Groups.[2].Value.[0] |> RuleType.RuleValue
        { Number = ruleNo; Rule = char}
    elif Regex.IsMatch(line, ruleLinkMutli) then
        let m = Regex.Match(line, ruleLinkMutli)
        let ruleNo = m.Groups.[1].Value |> int
        let firstPart = m.Groups.[2].Value.TrimEnd().Split(" ") |> Seq.map(fun s -> s.Trim() |> int) |> Seq.toList
        let sndPart = m.Groups.[4].Value.TrimEnd().Split(" ") |> Seq.map(fun s -> s.Trim() |> int) |> Seq.toList
        { Number = ruleNo; Rule = (firstPart, sndPart) |> RuleType.AlternativeLinks}
    else
        let m = Regex.Match(line, ruleLinkSingle)
        let ruleNo = m.Groups.[1].Value |> int
        let firstPart = m.Groups.[2].Value.TrimEnd().Split(" ") |> Seq.map(fun s -> s.Trim() |> int) |> Seq.toList |> RuleType.RuleLinks 
        { Number = ruleNo; Rule = firstPart}

let getInputParts ()= 
    let mapped = readLines("data\\19-01.txt")
                    |> splitSeq(fun s -> s = "")
                    |> Seq.toArray
    (mapped.[0],mapped.[1])

let countValidating validateFn messages = 
    messages |> PSeq.filter validateFn |> Seq.length

let solution1 () =
    let (rules, messages) = getInputParts()
    let rulesDict = rules |> Seq.map(parseRuleLine) |>  Seq.map(fun r -> (r.Number, r)) |> dict
    let firstRule = rulesDict.[0]
    let allMatching = ruleToStrings rulesDict firstRule 0
    let validateFn m = allMatching (Some m) |> Seq.contains m
    let res = messages |> countValidating validateFn
    res

let validateMessage2 (rule42mathches:HashSet<string>) (rule31matches:HashSet<string>) (message:string) =
    let len = message.Length
    let is42 m = rule42mathches.Contains(m)
    let is31 m = rule31matches.Contains(m)
    if  len%8<> 0 then false else
        let chunked = chunkStr 8 message
        let existsNotMatching = chunked 
                                    |> List.tryFind (fun m -> (is42 m) = false && (is31 m) = false)
                                    |> Option.isSome 
        if existsNotMatching then false else
            let countOfLeading42 = chunked |> List.fold(fun acc str -> if is42 str then (fst acc,snd acc+fst acc) else (0,snd acc)) (1,0) |> snd
            let countOfTailing31 = List.foldBack (fun str acc-> if is31 str then (fst acc,snd acc+fst acc) else (0,snd acc)) chunked  (1,0) |> snd
            (countOfTailing31 > 0) && (countOfLeading42 * 8 + countOfTailing31 * 8) = len && countOfLeading42 >= countOfTailing31 + 1 

let solution2 () =
    let (rules, messages) = getInputParts()
    let rulesDict = rules |> Seq.map(parseRuleLine) |>  Seq.map(fun r -> (r.Number, r)) |> dict
    let allMatchingRule rule = ruleToStrings rulesDict rule 0 None
    let rule42mathces = rulesDict.[42] |> allMatchingRule |> HashSet
    let rule31mathces = rulesDict.[31]|> allMatchingRule |> HashSet
    let isMatch message = validateMessage2 rule42mathces rule31mathces message
    messages |> countValidating isMatch