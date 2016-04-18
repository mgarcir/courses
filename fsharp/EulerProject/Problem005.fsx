open System
open System.Linq

let rec generatePrimeSeq list = function
        | hd :: tl ->
            let sive = hd :: list
            let filterList = tl |> List.filter (fun x -> x%hd<>0)
            generatePrimeSeq sive filterList
        |[] -> list

let maxMult = generatePrimeSeq [] [2..20] |> List.fold (fun acc x -> acc * x) 1
printfn "%A" maxMult

let seq = Seq.unfold (fun x -> Some(x, x+ 1)) 0
let filterSeq = seq |> Seq.find (fun x -> x = maxMult)
printfn "%A" filterSeq

//://stefanoricciardi.com/2010/08/12/project-euler-problem-5-in-f/
// http://theburningmonk.com/2010/09/project-euler-problem-5-solution/
