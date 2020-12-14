module utils 

open System.IO
open Microsoft.FSharp.Core.Operators.Checked


let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitSeq (splitFn) input =
    let i = ref 0
    input |>
        Seq.map(fun x ->
                    if splitFn x then incr i
                    !i, x) |>
        Seq.groupBy fst |>
        Seq.map(fun (_,x) -> Seq.map snd x |> Seq.filter (fun s -> not (splitFn s)))

let rec gcd a b = match (a,b) with
            | (x,y) when x = y -> x
            | (x,y) when x > y -> gcd (x-y) y
            | (x,y) -> gcd x (y-x)

let lcm a b = a*b/(gcd a b)  

let lcmOfSeq nums =
    nums |> Seq.fold(fun acc n -> lcm acc (int64 n)) 1L

let cartesian xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))