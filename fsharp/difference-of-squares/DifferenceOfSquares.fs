module DifferenceOfSquares

let squareOfSum (number: int): int =
    [ 1 .. number ]
    |> List.sum
    |> fun x -> pown x 2

let sumOfSquares (number: int): int =
    [ 1 .. number ]
    |> List.sumBy (fun i -> pown i 2)
    
let differenceOfSquares (number: int): int =
    squareOfSum number - sumOfSquares number