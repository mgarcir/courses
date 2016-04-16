open System

let limit = 100000

let rec generatePrimeSeq list = function
    | hd :: tl ->
         let sive = hd :: list
         let filterList = tl |> List.filter (fun x -> x%hd<>0)
         generatePrimeSeq sive filterList
    |[] -> list

let seqc = generatePrimeSeq [] [2..10000] |> List.rev
printfn "%A" seqc


let generatePrimeSeq' = Seq.unfold (fun (current, next) -> Some(next, (next, current + next))) (1,1)
generatePrimeSeq' |> Seq.takeWhile (fun x -> x < 1000) |> printfn "%A"

