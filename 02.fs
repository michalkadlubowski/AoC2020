module day2

open System.Text.RegularExpressions

type PassData = {min: int; max: int; character: char; password: string;}

let loadData = 
    readLines("data\\02-01.txt") |>
    Seq.map(fun s -> Regex.Match(s, @"(\d*)\-(\d*)\s(.):\s(\w+)")) |>
    Seq.map(fun r -> { 
        min = System.Int32.Parse(r.Groups.[1].Value);
        max =  System.Int32.Parse(r.Groups.[2].Value);
        character = r.Groups.[3].Value.[0];
        password = r.Groups.[4].Value})

let isValid  (passdata: PassData) = 
    let countOfChars = Seq.toArray passdata.password |>
                            Array.filter(fun c -> c.Equals(passdata.character)) |>
                            Array.length
    countOfChars >= passdata.min && countOfChars <= passdata.max

let isValidUpdated  (passdata: PassData) = 
    let first = passdata.password.ToCharArray().[passdata.min-1].Equals(passdata.character)
    let second = passdata.password.ToCharArray().[passdata.max-1].Equals(passdata.character)
    first <> second

let coutOfValid validationFn = loadData |>
                                    Seq.filter(validationFn) |>
                                    Seq.length
    