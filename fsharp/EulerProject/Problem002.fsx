open System

let rec fib a b =
    if a + b < 4000000 then
       let current = a + b
       current :: fib b current
    else
       []

let secFib = 1::2::fib 1 2
let even input = if input % 2 = 0 then true else false
let sumEven = secFib |> List.filter even |> List.sum

printfn "%A" secFib
printfn "%d" sumEven}

// the lazy list definition
let lazyList =
        Seq.unfold
                (fun x ->
                             if x < 13 then
                                 // if smaller than the limit return
                                 // the current and next value
                                 Some(x, x + 1)
                             else
                                // if great than the limit
                                // terminate the sequence
                                None)
                                        10

                                        // print the results
                                        printfn "%A" lazyList
                                        
