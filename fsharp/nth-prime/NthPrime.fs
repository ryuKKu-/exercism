module NthPrime

let isPrime (number: int) =
    seq { 2 .. sqrt(float number) |> floor |> int }
    |> Seq.forall (fun x -> number % x <> 0)

let prime nth =
    match nth with
    | 0 -> None
    | n ->
        (+) 2
        |> Seq.initInfinite
        |> Seq.filter isPrime
        |> Seq.item (n - 1)
        |> Some
