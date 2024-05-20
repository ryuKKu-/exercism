module RailFenceCipher

let encode (rails: int) (message: string) =
    let arr = Array2D.init rails message.Length (fun _ _ -> '.')
    
    seq {
        let mutable r = 0
        let mutable dir = false
        
        for i in 0 .. message.Length - 1 do
            if r = 0 || r = rails - 1 then
                dir <- not dir
            
            arr[r, i] <- message[i]
            
            r <- if dir then r + 1 else r - 1
               
        for i in 0 .. rails - 1 do
            yield arr[i,*] |> Array.filter (fun c -> c = '.' |> not)
    }
    |> Seq.concat
    |> Seq.toArray
    |> System.String
    
    
let decode (rails: int) (message: string) =
    let arr = Array2D.init rails message.Length (fun _ _ -> '.')

    let mutable r = 0
    let mutable dir = false
        
    for i in 0 .. message.Length - 1 do
        if r = 0 || r = rails - 1 then
            dir <- not dir
            
        arr[r, i] <- '*'
            
        r <- if dir then r + 1 else r - 1
        
    let mutable idx = 0
    for i in 0 .. rails - 1 do
        for j in 0 .. message.Length - 1 do
            if arr[i, j] = '*' && idx < message.Length then
                arr[i, j] <- message[idx]
                idx <- idx + 1
    
    seq {
        for i in 0 .. message.Length - 1 do
            printfn "%A" arr[*, i]
            yield arr[*, i] |> Array.filter (fun c -> c = '.' |> not)
    }
    |> Seq.concat
    |> Seq.toArray
    |> System.String
   
let roman arabicNumeral = 
    (([], arabicNumeral), [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")])
    ||> Seq.fold (fun (acc, n) (div, c) ->
        match n / div with
        | 0 -> (acc, n)
        | v ->
            let z = List.init v (fun _ -> c)
            (z @ acc, n - (v * div)))
    |> fst
    |> List.rev
    |> List.toArray
    |> String.concat ""
    
    // 123
    // 123 / 1000 = 0 -> 123 []
    // 123 / 500 = 0 -> 123 []
    // 123 / 100 = 1 -> 23 ['C']
    // 23 / 50 = 0 -> 23 ['C']
    // 23 / 10 = 2 -> 3 ['CXX']
    // 3 / 5 = 0 ->  3 ['CXX']
    // 3 / 1 = 3 -> 0 ['CXXIII']
    
    