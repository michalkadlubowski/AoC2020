module day21

open System
open System.Collections.Generic
open System.Text.RegularExpressions

type RecipeInfo = { Ingredients : string array; Allergens : string array }

let parseLine line =
    let m  = Regex.Match(line,@"(.*) \(contains(.*)\)")
    let ingredients = m.Groups.[1].Value.Split(" ") |> Seq.map(fun x -> x.Trim()) |> Seq.map(fun x -> x.TrimEnd()) |> Seq.toArray
    let alergens = m.Groups.[2].Value.Split(",") |> Seq.map(fun x -> x.Trim()) |> Seq.map(fun x -> x.TrimEnd()) |> Seq.toArray
    { Ingredients = ingredients; Allergens = alergens }

let getIngredientListsThatContainAllergen (recipes:RecipeInfo array) (allergen:string) =
    recipes
        |> Array.filter(fun r -> r.Allergens |> Array.contains(allergen))
        |> Array.map(fun r -> r.Ingredients)

let inBoth arr1 arr2 =
    arr1 
        |> Array.filter (fun t -> arr2 |> Array.exists (fun t2 -> t=t2))

let rec detectAllergens (boiledDownAllergens: (string * string array) list)  (acc:(string*string) list) =
    if boiledDownAllergens.Length = 0 then
        acc
    else
        let firstWithOne = boiledDownAllergens |> List.find(fun (_, ingList) -> ( ingList |> Array.length = 1 ))
        let detected = (fst firstWithOne, (snd firstWithOne).[0])
        let remaining = boiledDownAllergens
                            |> List.filter(fun x -> (fst detected) <> (fst x))
                            |> List.map(fun (allergen, ings) -> (allergen, ings |> Array.filter(fun i -> i <> (snd detected))))
        detectAllergens remaining (detected::acc)
    
let solution1 () =
    let mapped = readLines("data\\21-01.txt") |> Seq.map parseLine |> Seq.toArray
    let allAlergens = mapped |> Array.fold (fun acc curr -> List.append (curr.Allergens |> Array.toList) acc) List.empty<string> |> List.distinct
    let allergensWithPossibleIngredients = allAlergens |> List.map(fun x -> (x, getIngredientListsThatContainAllergen mapped x))
    let boiledDown = allergensWithPossibleIngredients
                        |> List.map(fun (allergen, ingredientLists) ->
                                        let boiled = ingredientLists |> Array.reduce inBoth
                                        (allergen, boiled))
    let allBoiledDownAllergens = boiledDown |> List.fold (fun acc curr -> Array.append (snd curr) acc) Array.empty<string> |> Array.distinct
    let allIngredients = mapped |> Array.fold (fun acc curr -> List.append (curr.Ingredients |> Array.toList) acc) List.empty<string> |> List.distinct
    let deffinetelyNotAllergens = allIngredients |> List.filter(fun x -> allBoiledDownAllergens |> Array.contains x = false) |> set
    mapped |> Seq.sumBy(fun r -> r.Ingredients |> Array.filter(fun i -> deffinetelyNotAllergens.Contains(i)) |> Array.length)

let solution2 () =
    let mapped = readLines("data\\21-01.txt") |> Seq.map parseLine |> Seq.toArray
    let allAlergens = mapped |> Array.fold (fun acc curr -> List.append (curr.Allergens |> Array.toList) acc) List.empty<string> |> List.distinct
    let allergensWithPossibleIngredients = allAlergens |> List.map(fun x -> (x, getIngredientListsThatContainAllergen mapped x))
    let boiledDown = allergensWithPossibleIngredients
                        |> List.map(fun (allergen, ingredientLists) ->
                                    let boiled = ingredientLists |> Array.reduce inBoth
                                    (allergen, boiled))
    let detected = detectAllergens boiledDown List.empty<string*string>
    let allergens = detected |> List.sortBy fst |>  List.map snd
    let joined = String.Join(',',allergens)
    printfn "%s" joined
    0