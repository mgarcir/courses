open System
open System.Linq


let int2String (x: int) = string x

let rec generatePin (pin:string) = function
    | [] -> pin
    | hd::tl ->
        let nextPin = int2String hd
        if pin.Contains(nextPin) then
            generatePin pin tl
        else
            generatePin (pin + nextPin) tl

generatePin String.Empty [1..9999] |> printfn "%s"
