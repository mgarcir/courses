(*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
*)

(*
let rec generatePrimeSeq list = function
 | hd :: tl ->
  let sive = hd :: list
  let filterList = tl |> List.filter (fun x -> x%hd<>0)
  generatePrimeSeq sive filterList
 |[] -> list

let primes = Seq.initInfinite (fun x -> x + 2) |> Seq.takeWhile (fun x -> (Seq.length x) < 100)*)

let isPrime n =
    let sqrt' = (float >> sqrt >> int) n // square root of integer
    [ 2 .. sqrt' ] // all numbers from 2 to sqrt'
    |> List.forall (fun x -> n % x <> 0) // no divisors

let lastSeqElement = Seq.fold (fun acc next -> next) 0 

let primes = Seq.initInfinite(fun x -> x+2)
             |> Seq.filter (fun x -> isPrime x)
             |> Seq.truncate 10001
             |> lastSeqElement
             |> printfn "%i"
