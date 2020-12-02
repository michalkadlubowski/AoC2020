module day1

open utils

let dataAsInts = 
    readLines("data\\01-01.txt")  |>
    Seq.map System.Int32.Parse

let targetValues = Set.ofSeq( dataAsInts |> Seq.map(fun x -> 2020 - x) )

let answers = dataAsInts |> Seq.filter(fun i -> targetValues.Contains(i));

let secondPartAnswers =
    let inputSet  = Set.ofSeq(dataAsInts) 
    let possibilities = dataAsInts |> 
        Seq.map(fun input ->  (input, Set.ofSeq(targetValues |> Seq.map(fun target ->  target - input) |> Seq.filter(fun x -> x > 0))))
    possibilities |> Seq.filter(fun p -> (snd p) |> Seq.exists(fun i -> inputSet.Contains(i))) |>
    Seq.map(fun pos -> (fst pos, (snd pos) |> Seq.find(fun x -> inputSet.Contains(x)))) |>
    Seq.map(fun x -> (fst x * snd x * (2020 - fst x - snd x)))
