(*
P11  Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

        Example:
            * (encode-modified '(a a a a b c c a a d e e e e))
                ((4 A) B (2 C) (2 A) D (4 E))
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

group seq |> compact |> printfn "%A"
