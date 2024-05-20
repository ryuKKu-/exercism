module RunLengthEncoding

open System

let encode (input: string) =      
    let rec loop acc (input: char list) =
        match input with
        | [] -> acc |> List.rev |> String.Concat
        | head :: tail ->
            let count = tail |> Seq.takeWhile(fun c -> c = head) |> Seq.length
            if count = 0 then
                loop ((sprintf "%c" head) :: acc) tail
            else
                loop ((sprintf "%d%c" (count + 1) head) :: acc) tail[count..]

    loop [] (input |> Seq.toList)
    
let decode input =
    let rec loop acc count (input: char list) =
        match input with
        | [] -> acc |> List.rev |> String.Concat
        | head :: tail when Char.IsDigit head ->
            let count = tail |> Seq.takeWhile(Char.IsDigit) |> Seq.toList
            loop acc (head :: count) tail[count.Length..]
        | head :: tail ->
            let str =
                if List.isEmpty count then string head
                else String.replicate (count |> String.Concat |> int) (string head)
            loop (str :: acc) [] tail
    
    loop [] [] (input |> Seq.toList)