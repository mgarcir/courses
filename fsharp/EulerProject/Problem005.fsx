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

let odds = 2:: [3..2..20]
let divisibleBy x y = if (x % y = 0) then true else false
let divisibleByOdds index = odds |> List.forall (fun x -> divisibleBy index x) 
let seq = Seq.unfold (fun x -> Some(x, x+ 1)) maxMult
let filterSeq = seq |> Seq.find (fun x -> divisibleByOdds x)

printf "%A" odds
//seq |> Seq.takeWhile (fun x -> x < 100) |> printfn "%A"
//seq |> Seq.find (fun x -> x = 20) |> printfn "%A"
printfn "%A" filterSeq
