open System

let x =
   [for x in 1..1000 do
    if x%3 = 0 || x%5 = 0 then yield(x)] |> List.sum

//BurningMonk
let y =[1..1000] |> List.filter (fun x -> x%3 = 0 || x%5 = 0) |> List.sum

//Gauss
let z1 = ([1..333] |> List.sum) * 3
let z2 = ([1..199] |> List.sum) * 5
let z3 = ([1..66]  |> List.sum) * 15

let z = z1 + z2 - z3

printfn "%d" x
printfn "%d" y
printfn "%d" z
