module utils 

open System.IO

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