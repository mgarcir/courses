open System


let list = [1..33..1000]

list |> List.rev |> printfn "%A"


let rec reverseList reversed = function
    | [] -> reversed
    | hd::tl -> reverseList  (hd::eversed) tl

reverseList [] list |> printfn "%A"
