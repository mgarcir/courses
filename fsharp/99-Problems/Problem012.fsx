(*
P12  Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

   *)
   
open System

let seq = [1;1;1;1;2;2;3;4;4;]

let group xs =
    let collect next = function
        | [cnt;element]::ys when element = next -> [(cnt+1);element]::ys
        | xs -> [1;next]::xs
    List.fold (fun acc x -> collect x acc) [] xs

let compact xs =
    let loop next xs =
        match next with
        | [] -> xs
        | 1::tl -> tl :: xs
        | tl -> tl::xs
    List.fold (fun acc x -> loop x acc) [] xs

let unCompact xs =
    let loop next xs =
        match next with
            | [] -> xs
            | [element] -> [1;element]::xs
            | hd::element -> next::xs
    List.fold (fun acc x -> loop x acc) [] xs

group seq |> compact |> unCompact |> printfn "%A"
