(*
The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)
let test = 1 |> (fun x -> x*x)
let sumOfSquares = [1..100] |> List.fold (fun acc x -> acc + (x*x)) 0
let squareOfSum = [1..100] |> List.sum |> (fun x -> x*x)
let difference = sumOfSquares - squareOfSum 

printfn "%i" difference
