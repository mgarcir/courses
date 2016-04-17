open System

let deck = [for x in [1..10] do
            for y in ["A";"B";"C";"D"] do
            yield (x,y)]

type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
                    Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

let ramdom = System.Random()

let getCardFromDeck deck = ramdom.GetValues(0, List.length deck) |> Seq.head

let calculateScore actualScore playerCard otherPlayerCard =
    if((fst playerCard)> (fst otherPlayerCard)) then
        actualScore + 1
    else
        actualScore

let printResult playerCard1 playerCard2 newPlayerOneScore newPlayerTwoScore =
    printfn "Player One Card is %i of %s" (fst playerCard1) (snd playerCard1)
    printfn "Player Two Card is %i of %s" (fst playerCard2) (snd playerCard2)
    printfn "Player One Score is %i" newPlayerOneScore
    printfn "Player Two Score is %i" newPlayerTwoScore

    
let rec play playerOneScore playerTwoScore = function
    | [] -> if playerOneScore > playerTwoScore then "Player One Wins!!" else "Computer Wins"
    | deck ->
        let player1Card = List.nth deck (getCardFromDeck deck)
        let player2Card = List.nth deck (getCardFromDeck deck)
        if (player1Card = player2Card) then
           play playerOneScore playerTwoScore deck
        else
         let filterDeck =  deck |> List.filter (fun x -> x <> player1Card && x <> player2Card)
         let newPlayerOneScore = calculateScore playerOneScore player1Card player2Card
         let newPlayerTwoScore = calculateScore playerTwoScore player2Card player1Card
         printResult player1Card player2Card newPlayerOneScore newPlayerTwoScore
         play (newPlayerOneScore) (newPlayerTwoScore) filterDeck



let playGame = play 0 0 deck

printfn "%s" playGame


