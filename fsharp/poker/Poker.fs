module Poker

open System

type Suit =
    | Heart
    | Spade
    | Diamond
    | Club
    
type CardRank =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    
    member this.value =
        match this with
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten -> 10
        | Jack -> 11
        | Queen -> 12
        | King -> 13
        | Ace -> 14
        
type Card = Suit * CardRank

type HandRank =
    | HighestCard
    | Pair
    | TwoPair
    | ThreeOfKind
    | Straight
    | Flush
    | FullHouse
    | FourOfKind
    | StraightFlush

[<Struct>]  
type RankedHand = {
    Raw: string
    Rank: HandRank
    Cards: CardRank list
}
with
    static member create hand rank cards =
        { Rank = rank; Cards = cards; Raw = hand }
    
    static member empty =
        { Rank = HighestCard; Cards = []; Raw = "" }
        
      
module Parser =
    let private parseSuit c =
        match c with
            | 'H' -> Heart
            | 'S' -> Spade
            | 'C' -> Diamond
            | 'D' -> Club
            | _ -> failwith "Invalid suit"
            
    let private parseRank c =
        match c with
        | "2" -> Two
        | "3" -> Three
        | "4" -> Four
        | "5" -> Five
        | "6" -> Six
        | "7" -> Seven
        | "8" -> Eight
        | "9" -> Nine
        | "10" -> Ten
        | "J" -> Jack
        | "Q" -> Queen
        | "K" -> King
        | "A" -> Ace
        | _ -> failwith "Invalid rank"
    
    let private parseCard (str: string) =
        parseSuit str[str.Length - 1], parseRank (str[0 .. str.Length - 2])
        
    let parseHand (raw: string) =
        let cards = raw.Split(" ", StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map parseCard
                    |> Array.toList
        
        if cards.Length < 5 then
            failwith "Hand of 5 cards required"
        else
            raw, cards

module Evaluator =
    let (|IsStraight|_|) (cards: CardRank list) =
        if cards = [ Ace; Five; Four; Three; Two ] then
            Some [ Five; Four; Three; Two; Ace ]
        else
            let isStraight =
                cards
                |> Seq.pairwise
                |> Seq.forall (fun (r1, r2) -> r2.value = r1.value - 1)
            
            if isStraight then
                Some cards
            else
                None
               
    let rankHand (raw, cards: Card list) =
        let isSingleSuit = cards |> Seq.groupBy fst |> Seq.length = 1
        let orderedByRankDesc = cards |> Seq.sortByDescending snd |> Seq.map snd |> List.ofSeq
        
        let multiples, remaining =
                orderedByRankDesc
                |> List.countBy id
                |> List.sortByDescending(fun x -> x |> snd, x |> fst)
                |> List.partition (fun (_, i) -> i >= 2)
              
        let remainder = remaining
                        |> Seq.map fst
                        |> Seq.sortDescending
                        |> List.ofSeq
        
        let rank, cards = 
            match isSingleSuit, orderedByRankDesc, multiples with
            | true, IsStraight cardRanks, _ -> StraightFlush, cardRanks
            | true, _, _ -> Flush, orderedByRankDesc
            | _, IsStraight cardRanks, _ -> Straight, cardRanks
            | _, _, [(cr, 2)] -> Pair, cr :: remainder
            | _, _, [(cr, 3)] -> ThreeOfKind, cr :: remainder
            | _, _, [(cr, 4)] -> FourOfKind, cr :: remainder
            | _, _, [(cr1, 2); (cr2, 2)] -> TwoPair, ([cr1; cr2] @ remainder)
            | _, _, [(cr1, 3); (cr2, 2)] -> FullHouse, ([cr1; cr2] @ remainder)
            | _, _, _ -> HighestCard, remainder

        RankedHand.create raw rank cards
   
let bestHands (hands: string list) =
        hands
        |> Seq.map (Parser.parseHand >> Evaluator.rankHand)
        |> Seq.fold (fun (currMax, list) hand ->
            if hand.Rank > currMax.Rank then
                hand, [hand.Raw]
            else if hand.Rank = currMax.Rank then
                if hand.Cards = currMax.Cards then
                    currMax, hand.Raw :: list
                else if hand.Cards > currMax.Cards then
                    hand, [hand.Raw]
                else
                    currMax, list
            else
                currMax, list) (RankedHand.empty, [])
        |> snd
        |> List.rev