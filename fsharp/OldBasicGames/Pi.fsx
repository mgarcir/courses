open System
open System.Linq
open Microsoft.FSharp.Math

let pi50Decimals = "3.14159265358979323846264338327950288419716939937510"

let generateNextDecimals init  = pi50Decimals.Substring(init - 4, init)
let piDecimals length = pi50Decimals.Substring(0, length)

let rec mainLoop index =
    printf "Next decimals are %s\n" (generateNextDecimals index)
    printf "\nInsert %i pi digits: " index
    let userResponse = Console.ReadLine()

    if userResponse = "n" then  // TODO: cleanup and exit
        printfn "Bye!!"
        //System.AppDomain.CurrentDomain.ProcessExit()
    else    
      let pi = piDecimals index
      
      if userResponse = pi then
          printfn "Correct."
          mainLoop (index + 2)
      else
          printfn "Incorrect. Response was: %s" pi 
          mainLoop index

printf "Wellcome to Pi Games!!!\n"
printf "Write all pi digits you can learn...\n"
mainLoop 4
