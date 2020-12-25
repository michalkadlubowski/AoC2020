module day25

open System

let rec transform subjecNumber loopSize value= 
    if loopSize = 0 then 
        value
    else
        let multiplied = value * (int64 subjecNumber)
        let (_,rem) = Math.DivRem(multiplied,20201227L)
        transform subjecNumber (loopSize-1) rem

let rec findLoopSize subjectNumber publicKey value currentLoopSize =
    if value = publicKey then currentLoopSize
    else
        let rem = transform subjectNumber 1 value
        findLoopSize subjectNumber publicKey (int64 rem) (currentLoopSize+1)

let solution1 () =
    let numbers = readLines "data\\25-01.txt" |> Seq.toArray
    let finder = findLoopSize 7L
    let doorPublicKey = numbers.[0] |> int64
    let cardPublicKey = numbers.[1] |> int64
    let doorLoopSize = finder doorPublicKey 1L 0
    let cardLoopSize = finder cardPublicKey 1L 0
    let res = transform doorPublicKey cardLoopSize 1L
    res

let solution2 () =
    0