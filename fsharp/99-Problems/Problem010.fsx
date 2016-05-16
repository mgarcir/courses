(*
   P10  Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

    Example:
    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

*)

open System

let seq = [1;1;1;1;2;2;3;4;4;]

let group xs =
    let collect next = function
        | [cnt;element]::ys when element = next -> [(cnt+1);element]::ys
        | xs -> [1;next]::xs
    List.fold (fun acc x -> collect x acc) [] xs

group seq |> printfn "%A"
