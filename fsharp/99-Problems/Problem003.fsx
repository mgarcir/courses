open System

let list = [1..999]

let getElement list index = List.nth list (index-1)

let rec getElement' list index =
    match list, index with
        | [],_ -> failwith "Empty"
        | _,1 -> List.head list
        | hd::tl,index ->  getElement' tl (index - 1)  

getElement list 33 |> printfn "%d"
getElement' list 33 |> printfn "%d"
