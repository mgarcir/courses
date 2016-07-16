(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
   *)
   
let calculateSieveEratosthenes upperLimit =
  let rec crib primeList = function
    | [] -> primeList
    | hd::tl ->
        let tailFiltered = tl |> List.filter (fun x -> x%hd <> 0)
        let updatedPrimeList = hd::primeList
        crib updatedPrimeList tailFiltered
  crib [] [2..upperLimit]

calculateSieveEratosthenes 10 |> printfn "%A"
calculateSieveEratosthenes 10 |> List.fold (fun acc next -> acc + next) 0
                              |> printfn "%d"
