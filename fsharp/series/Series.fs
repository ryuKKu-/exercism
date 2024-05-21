module Series

let slices (str: string) length =
    if str.Length = 0 || length < 1 || length > str.Length then
        None
    else 
        str
        |> Seq.windowed length
        |> Seq.map(System.String)
        |> Seq.toList
        |> Some