open System

let list = [1..1000]

List.length list |> printfn "%d"

let rec countLength acc = function
    | [] -> acc
    | hd::tl -> countLength (acc + 1) tl

countLength 0 list |> printfn "%d"
