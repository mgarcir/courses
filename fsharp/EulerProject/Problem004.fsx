open System
open System.Linq

let list = [for x in 100..999 do
            for y in 100..999 do
            yield (x*y)]

let isPalindromic number =
        let charArray = number.ToString().ToCharArray()
        let reverseCharArray = Array.rev charArray
        charArray.SequenceEqual(reverseCharArray)

list |> List.filter isPalindromic |> List.rev |> printfn "%A"
