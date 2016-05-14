(*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

open System.Linq

let triples = [for a in 1..998 do
               for b in 1..998 do
               for c in 1..998 do
               if a + b + c = 1000 && a < b && b < c then
                  yield [a;b;c;]]

let triples' = [for n in 1 .. 999 do
                for m in 1 .. 999 do
                let a = (m*m) - (n*n)
                let b = 2*m*n
                let c = (m*m) + (n*n)
                if (m > n) && a + b + c = 1000 then
                 yield [a;b;c]]

let filterPythagoreanTriples acc = function
    | [a;b;c] when (a*a) + (b*b) = (c*c) -> (a*b*c)::acc
    | _ -> acc


let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let pythagoreanTriple = triples |> List.fold (fun acc next -> filterPythagoreanTriples acc next) []

printfn "%A" pythagoreanTriple

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds


let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()

let pythagoreanTriple' = triples' |> List.fold (fun acc next -> filterPythagoreanTriples acc next) []

printfn "%A" pythagoreanTriple'

stopWatch2.Stop()
printfn "%f" stopWatch2.Elapsed.TotalMilliseconds

let duration f =
 let timer = new System.Diagnostics.Stopwatch()
 timer.Start()
 let returnValue = f()
 printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
 returnValue
