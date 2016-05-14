open System
open System.Linq

type 'a NestedList = List of 'a NestedList list | Elem of 'a
let listToFlat = (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]])

printfn "%A" listToFlat

let rec flatter listFlatted = function
    | Elem x -> [x]
    | List xs -> xs |> List.foldBack (fun acc y -> flatter acc x) acc xs 
