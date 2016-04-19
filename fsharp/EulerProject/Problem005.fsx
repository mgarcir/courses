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

let odds = 1::2:: [3..5..10]
let divisibleBy x y = if (x % y = 0) then true else false
let divisibleByOdds index = odds |> List.filter (fun x -> divisibleBy index x) 
let seq = Seq.unfold (fun x -> Some(x, x+ 1)) 11
let filterSeq = seq |> Seq.find (fun x -> (List.length (divisibleByOdds x)) = 6)
printfn "%A" filterSeq

//://stefanoricciardi.com/2010/08/12/project-euler-problem-5-in-f/
// http://theburningmonk.com/2010/09/project-euler-problem-5-solution/

let isEvenlyDivided(n, m) = n % m = 0
let isEvenlyDividedByAll(n, numbers) = numbers |> Seq.forall (fun x -> isEvenlyDivided(n, x))

 let findSmallestCommonMultiple(numbers) =
         let max = Array.max(numbers)
             Seq.unfold (fun x -> Some(x, x + 1)) max
                 |> Seq.filter (fun x -> isEvenlyDividedByAll(x, numbers))
                     |> Seq.head
                      
