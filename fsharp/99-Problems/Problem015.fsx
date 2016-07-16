(*
P15 (**) Replicate the elements of a list a given number of times.
Example:
 (repli '(a b c) 3)
 (A A A B B B C C C)
*)

let listToReplicate = [1;2;3;4;]

let rec replicateElement acc times =
    match times with
    | x when (x<=1) -> acc
    | x ->
          let newAcc = (List.head acc)::acc
          let timesReminder = times - 1
          replicateElement newAcc timesReminder

List.fold (fun acc next -> (replicateElement [next] next)::acc) [] listToReplicate |> List.rev
 |> printfn "%A"
