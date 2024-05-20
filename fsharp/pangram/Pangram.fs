module Pangram

let isPangram (input: string): bool =
    input.ToLowerInvariant()
    |> Seq.filter(fun c -> int c >= int 'a' && int c <= int 'z')
    |> Seq.distinct
    |> Seq.length = 26
