open System
open System.Linq

let listToTest = [[1;2;3;4;3;2;1]; [1;2;3;]]

//Framework mode
let isPalindrome list = if (list = (List.rev list)) then true else false

listToTest |> List.iter (fun x -> printf "%b" (isPalindrome x))

//RecursiveMode
let isPalindromeRec list =
    let rec loop = function
        | [] -> true
        | hd::tl ->
            let headList = hd
            let headInvList = tl |> List.rev |> List.head
            if (headList) <> (headInvList) then false else loop tl
    loop list

listToTest |> List.iter (fun x -> printf "%b" (isPalindromeRec x))
