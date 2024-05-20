module RailFenceCipherTests

open FsUnit.Xunit
open Xunit

open RailFenceCipher

let roman arabicNumeral = 
    (([], arabicNumeral), [(1000, 'M'); (500, 'D'); (100, 'C'); (50, 'L'); (10, 'X'); (5, 'V'); (1, 'I')])
    ||> Seq.fold (fun (acc, n) (div, c) ->
        match n / div with
        | 0 -> (acc, n)
        | v ->
            let z = List.init v (fun _ -> c)
            (z @ acc, n - (v * div)))
    |> fst
    |> List.rev
    |> List.toArray
    |> System.String

[<Fact>]
let ``Encode with two rails`` () =
    roman 123 |> should equal "CXXIII"

[<Fact>]
let ``Encode with three rails`` () =
    let rails = 3
    let msg = "WEAREDISCOVEREDFLEEATONCE"
    let expected = "WECRLTEERDSOEEFEAOCAIVDEN"
    encode rails msg |> should equal expected

[<Fact>]
let ``Encode with ending in the middle`` () =
    let rails = 4
    let msg = "EXERCISES"
    let expected = "ESXIEECSR"
    encode rails msg |> should equal expected

[<Fact>]
let ``Decode with three rails`` () =
    let rails = 3
    let msg = "TEITELHDVLSNHDTISEIIEA"
    let expected = "THEDEVILISINTHEDETAILS"
    decode rails msg |> should equal expected

[<Fact>]
let ``Decode with five rails`` () =
    let rails = 5
    let msg = "EIEXMSMESAORIWSCE"
    let expected = "EXERCISMISAWESOME"
    decode rails msg |> should equal expected

[<Fact>]
let ``Decode with six rails`` () =
    let rails = 6
    let msg = "133714114238148966225439541018335470986172518171757571896261"
    let expected = "112358132134558914423337761098715972584418167651094617711286"
    decode rails msg |> should equal expected

