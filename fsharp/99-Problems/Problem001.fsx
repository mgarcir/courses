open System

let list = [1..100]

list |> List.rev |> List.head |> printfn "%d"

let rec myLast = function
    | [] -> failwith "Empty list!!"
    | [x] -> x
    | hd::tl -> myLast tl


list |> myLast |> printfn "%d"


list |> List.reduce (fun acc x -> x) |> printfn "%d"
