open System

let seq = Seq.unfold (fun x -> Some((0.5 * x), x + 1.0)) 1.0

seq |> Seq.takeWhile (fun x -> x < 1000.0) |> printfn "%A"


let limit = 100000

let rec generatePrimeSeq list = function
    | hd :: tl ->
         let sive = hd :: list
         let filterList = tl |> List.filter (fun x -> x%hd<>0)
         generatePrimeSeq sive filterList
    |[] -> list

let seqc = generatePrimeSeq [] [2..10000] |> List.rev
printfn "%A" seqc
