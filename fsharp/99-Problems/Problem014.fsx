(*
P14  Duplicate the elements of a list.
    Example:
    * (dupli '(a b c c d))
    (A A B B C C C C D D)

   *)

open System

let seq = ["A"; "B"; "C"; "D"]

let rec multiplicate elements numberOfTimes =
    match numberOfTimes with
        | x when x <= 1 -> elements
        | _ -> let element = Seq.head elements
               multiplicate (element::elements) (numberOfTimes - 1)

let rec multiplicateSeqElement acc numberOfTimes = function
    | [] -> List.rev acc
    | hd::tl ->
        let newAcc = (multiplicate [hd] numberOfTimes)::acc
        multiplicateSeqElement newAcc numberOfTimes tl

multiplicateSeqElement [] 2 [1] |> printfn "%A"

seq |> List.fold (fun acc next -> (multiplicate [next] 2)::acc) []
    |> List.rev
    |> printfn "%A"
