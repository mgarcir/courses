open System

let listToFilter = [1;1;1;2;3;1;3;3;3;4;5;4;4;5;5;]

let rec loop filterList = function
    | [] -> List.rev filterList
    | hd::tl ->
        let headFilterList = List.head filterList
        if (hd = headFilterList) then
            loop filterList tl
        else
            loop (hd::filterList) tl

loop [1] listToFilter |> printfn "%A"
