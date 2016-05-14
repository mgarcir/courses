open System

let seq = [1;1;1;1;2;2;3;4;4;]

(*

let rec groupSeq seqs = function
    | [] -> seqs
    | [fs;sd;_] ->
        if(fs = sd) then

       else
*)
(*
let filterSeq nextElement = function
    | [] -> [nextElement]
    | hd::tl ->
        if( nextElement = hd) then
            hd::tl
        else
            nextElement::hd::tl
    
List.foldBack (fun  x acc -> filterSeq x acc) seq [] |> printfn "%A"
List.fold (fun acc x -> filterSeq x acc) [] seq |> printfn "%A"
*)

let pack xs =
     let collect x = function
         | (y::xs)::tl when x = y -> (x::y::xs)::tl
         | xs -> [x]::xs
     List.foldBack collect xs []


let group xs =
    let collect next = function
        | [cnt;element]::ys when element = next -> [(cnt+1);element]::ys
        | xs -> [1;next]::xs
    List.fold (fun acc x -> collect x acc) [] xs

group seq |> printfn "%A"
