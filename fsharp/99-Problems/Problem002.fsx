open System

let list = [1..9999]

let rec lastButOne xs =
    if(List.length xs = 2) then List.head xs else lastButOne (List.tail xs)

list |> lastButOne |> printfn "%d" 

let rec lastButOne' = function
    | [] -> failwith "EmptyList"
    | [x] -> x
    | [x;y] -> x
    | hd::tl -> lastButOne' tl

list |> lastButOne' |> printfn "%d"


list |> List.rev |> List.tail |> List.head |> printfn "%d"

let lastButOne'' list =
    let flip f a b = f b a
    list |> flip List.nth 1 |> printfn "%d"
