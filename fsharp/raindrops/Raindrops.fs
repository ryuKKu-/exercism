module Raindrops

let convert (number: int): string =
    let table = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    
    let rec loop acc table =
        match table with
        | [] -> if List.isEmpty acc then (string number :: acc) else acc
        | (value, sound) :: tail ->
            if number % value = 0 then
                loop (sound :: acc) tail
            else
                 loop acc tail
                 
    loop [] table
    |> List.rev
    |> List.toArray
    |> String.concat ""