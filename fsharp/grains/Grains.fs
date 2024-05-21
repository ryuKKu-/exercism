module Grains

let square (n: int): Result<uint64,string> =
    if n < 1 || n > 64 then
        Error "square must be between 1 and 64"
    else if n = 1 then Ok 1UL
    else
        pown 2UL (n - 1) |> Ok
    
let total: Result<uint64,string> =
    [ 1 .. 64 ]
    |> List.sumBy(fun i -> square i |> Result.defaultValue 0UL)
    |> Ok