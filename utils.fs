[<AutoOpen>]
module utils 

open System.IO
open Microsoft.FSharp.Core.Operators.Checked

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readLine (filePath:string) =
    use sr = new StreamReader (filePath)
    sr.ReadLine ()

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

let rec cartesianLstLst lstlst =
    match lstlst with
    | h::[] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesianLstLst t)
    | _ -> []    

let product (seq1:'a seq) (seq2:'a seq seq) =
    seq { for item1 in seq1 do
              for item2 in seq2 do
                  yield item1 |> Seq.singleton |> Seq.append item2 }

let productn (s:seq<#seq<'a>>) =
    s |> Seq.fold (fun r s -> r |> product s) (seq { yield Seq.empty })

let chunkStr size str =
    let rec loop (s:string) accum =
        let branch = size < s.Length
        match branch with
        | true  -> loop (s.[size..]) (s.[0..size-1]::accum)
        | false -> s::accum
    (loop str []) |> List.rev    