module Matrix

let createMatrix (str: string) =
    str.Split("\n") |> Array.map (fun s -> s.Split(" ") |> Array.map int)

let row index (matrix: string) =   
    createMatrix matrix
    |> fun m -> m[index - 1]
    |> Array.toList

let column index (matrix: string) =
    createMatrix matrix
    |> Array.map (fun f -> f[index - 1])
    |> Array.toList
