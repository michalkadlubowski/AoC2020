module utils 

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitSeq (splitToken : string) input =
    let i = ref 0
    input |>
        Seq.map(fun x ->
                    if x = splitToken then incr i
                    !i, x) |>
        Seq.groupBy fst |>
        Seq.map(fun (_,x) -> Seq.map snd x |> Seq.filter (fun s -> s<> splitToken))